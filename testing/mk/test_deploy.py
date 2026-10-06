"""Deployment failures must not be reported as successful releases."""

import os
import sys
import unittest

import test_stardict


class DeployTests(unittest.TestCase):
    def setUp(self):
        self.fixture = test_stardict.StarDictTests()
        self.fixture.setUp()
        self.addCleanup(self.fixture.doCleanups)
        self.fixture.successful('release-stardict')
        self.root = self.fixture.root
        self.remote = self.root / 'remote with spaces'
        self.remote.mkdir()
        self.bin = self.root / 'bin'
        self.bin.mkdir()
        self.env = {'PATH': str(self.bin) + os.pathsep + os.environ['PATH'],
                    'REMOTE_ROOT': str(self.remote), 'REVIEW_LOG': str(self.root / 'remote-calls')}
        helper = self.bin / 'fd_file_mgr'
        helper.write_text(f'''#!{sys.executable}
import os, subprocess, sys
from pathlib import Path
def log(message):
    with Path(os.environ['REVIEW_LOG']).open('a') as out:
        out.write(message + '\\n')
if sys.argv[1] == '-r':
    log('path')
    print(os.environ['REMOTE_ROOT'])
    sys.exit(int(os.environ.get('PATH_STATUS', '0')))
assert sys.argv[1] == '--run'
log('mount')
mount = int(os.environ.get('MOUNT_STATUS', '0'))
if mount not in (0, 201):
    sys.exit(mount)
status = subprocess.run(sys.argv[2:], check=False).returncode
cleanup = 0
if mount != 201:
    log('cleanup')
    cleanup = int(os.environ.get('CLEANUP_STATUS', '0'))
sys.exit(status or cleanup)
''')
        helper.chmod(0o755)
        copy = self.bin / 'cp'
        copy.write_text(f'''#!{sys.executable}
import os, sys
from pathlib import Path
with Path(os.environ['REVIEW_LOG']).open('a') as out:
    out.write('copy ' + sys.argv[2] + '\\n')
kind = 'checksum' if sys.argv[2].endswith('.sha512') else 'archive'
if os.environ.get('FAIL_COPY') == kind:
    sys.exit(9)
os.execv('/bin/cp', ['/bin/cp', *sys.argv[1:]])
''')
        copy.chmod(0o755)

    def deploy(self, *arguments, **env):
        return self.fixture.make('deploy-stardict', *arguments, **dict(self.env, **env))

    def calls(self):
        return (self.root / 'remote-calls').read_text()

    def test_mount_failure_prevents_destination_access(self):
        result = self.deploy(MOUNT_STATUS='8')
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.calls(), 'mount\n')
        self.assertEqual(list(self.remote.iterdir()), [])

    def test_success_copies_archive_and_checksum_and_cleans_up(self):
        result = self.deploy()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        files = list((self.remote / 'eng-deu/1.0.0').iterdir())
        self.assertEqual(len(files), 2)
        self.assertTrue(any(path.name.endswith('.sha512') for path in files))
        self.assertTrue(self.calls().endswith('cleanup\n'))

    def test_existing_mount_is_preserved(self):
        self.assertEqual(self.deploy(MOUNT_STATUS='201').returncode, 0)
        self.assertNotIn('cleanup', self.calls())

    def test_copy_failures_publish_no_partial_release(self):
        for kind in ['archive', 'checksum']:
            with self.subTest(kind=kind):
                result = self.deploy(FAIL_COPY=kind)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(list((self.remote / 'eng-deu/1.0.0').iterdir()), [])
                self.assertTrue(self.calls().endswith('cleanup\n'))

    def test_destination_path_failure_and_empty_path_stop_copying(self):
        for env in [{'PATH_STATUS': '9'}, {'REMOTE_ROOT': ''}]:
            with self.subTest(env=env):
                (self.root / 'remote-calls').write_text('')
                self.assertNotEqual(self.deploy(**env).returncode, 0)
                self.assertNotIn('copy', self.calls())
                self.assertTrue(self.calls().endswith('cleanup\n'))

    def test_cleanup_failure_is_reported(self):
        self.assertNotEqual(self.deploy(CLEANUP_STATUS='7').returncode, 0)

    def test_force_replacement_preserves_previous_files_on_copy_failure(self):
        self.assertEqual(self.deploy().returncode, 0)
        destination = self.remote / 'eng-deu/1.0.0'
        before = {path.name: path.read_bytes() for path in destination.iterdir()}
        self.assertNotEqual(self.deploy('FORCE=y', FAIL_COPY='checksum').returncode, 0)
        self.assertEqual({path.name: path.read_bytes() for path in destination.iterdir()}, before)
        self.assertNotEqual(self.deploy().returncode, 0)


    def test_deployed_files_are_publicly_readable(self):
        archive = next((self.fixture.dictionary / 'build/release').glob('*.tar.xz'))
        archive.chmod(0o600)
        archive.with_name(archive.name + '.sha512').chmod(0o600)
        self.assertEqual(self.deploy().returncode, 0)
        for path in (self.remote / 'eng-deu/1.0.0').iterdir():
            self.assertEqual(path.stat().st_mode & 0o444, 0o444)

    def use_real_manager(self, strategy):
        generated = self.root / 'generated'
        generated.mkdir()
        conf = self.root / 'freedictrc'
        conf.write_text(f'''[DEFAULT]
file_access_via = {strategy}
[release]
local_path = {self.remote}
[generated]
local_path = {generated}
[crafted]
local_path = {self.fixture.dictionary}
''')
        self.env['TEST_FREEDICTRC'] = str(conf)
        helper = self.bin / 'fd_file_mgr'
        helper.write_text(f'''#!{sys.executable}
import os, sys
from pathlib import Path
sys.path.insert(0, {str(test_stardict.REPO / 'fd_tool')!r})
from fd_tool.scripts import fd_file_mgr
fd_file_mgr.config.discover_and_load = lambda: fd_file_mgr.config.load_configuration(os.environ['TEST_FREEDICTRC'])
fd_file_mgr.os.path.ismount = lambda path: Path(path, '.mounted').exists()
fd_file_mgr.main()
''')
        backend = f'''#!{sys.executable}
import os, sys
from pathlib import Path
name = Path(sys.argv[0]).name
path = Path(sys.argv[-1])
with Path(os.environ['REVIEW_LOG']).open('a') as out:
    out.write(name + ' ' + str(path) + '\\n')
if name == 'sshfs':
    if path.name == 'generated' and os.environ.get('FAIL_GENERATED'):
        sys.exit(8)
    (path / '.mounted').touch()
elif name == 'fusermount':
    (path / '.mounted').unlink()
'''
        for name in ['sshfs', 'fusermount', 'unison']:
            executable = self.bin / name
            executable.write_text(backend)
            executable.chmod(0o755)
        return generated

    def test_actual_manager_rolls_back_partial_sshfs_setup(self):
        self.use_real_manager('sshfs')
        result = self.deploy(FAIL_GENERATED='1')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('fusermount ' + str(self.remote), self.calls())
        self.assertNotIn('copy ', self.calls())
        self.assertEqual(list(self.remote.iterdir()), [])

    def test_actual_manager_preserves_borrowed_sshfs_mount(self):
        generated = self.use_real_manager('sshfs')
        (self.remote / '.mounted').touch()
        result = self.deploy()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertTrue((self.remote / '.mounted').exists())
        self.assertFalse((generated / '.mounted').exists())
        self.assertNotIn('fusermount ' + str(self.remote), self.calls())

    def test_actual_unison_manager_uploads_both_sections(self):
        generated = self.use_real_manager('unison')
        result = self.deploy()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        calls = [line for line in self.calls().splitlines() if line.startswith('unison ')]
        self.assertEqual(calls, ['unison ' + str(path)
                                for path in [self.remote, generated, generated, self.remote]])
        self.assertEqual(len(list((self.remote / 'eng-deu/1.0.0').iterdir())), 2)
