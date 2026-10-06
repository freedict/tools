"""Make API workflow checks without mounting or contacting remote services."""

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

REPO = Path(__file__).resolve().parents[2]


class ApiMakeTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.env = dict(os.environ, PATH=f'{self.root}{os.pathsep}{os.environ["PATH"]}',
                        REVIEW_LOG=str(self.root / 'calls'), API_DIR=str(self.root / 'api dir'))
        script = f'''#!{sys.executable}
import os, subprocess, sys
from pathlib import Path
name = Path(sys.argv[0]).name
def log(message):
    with open(os.environ['REVIEW_LOG'], 'a') as f:
        f.write(message + '\\n')
if name == 'fd_file_mgr' and sys.argv[1] == '--run':
    log('fd_file_mgr -m')
    status = int(os.environ.get('MOUNT_STATUS', '0'))
    if status not in (0, 201):
        sys.exit(status)
    child = subprocess.run(sys.argv[2:], check=False).returncode
    cleanup = 0
    if status != 201:
        log('fd_file_mgr -u')
        cleanup = int(os.environ.get('CLEANUP_STATUS', '0'))
    sys.exit(child or cleanup)
log(name + ' ' + ' '.join(sys.argv[1:]))
if name == 'fd_file_mgr':
    if sys.argv[1] in ('-a', '-r'):
        print(os.environ['API_DIR'])
        sys.exit(int(os.environ.get('PATH_STATUS', '0')))
    if sys.argv[1] == '-m':
        sys.exit(int(os.environ.get('MOUNT_STATUS', '0')))
    sys.exit(0)
sys.exit(int(os.environ.get('API_STATUS', '0')) if name == 'fd_api' else 0)
'''
        for name in ['fd_api', 'fd_file_mgr', 'validator']:
            path = self.root / name
            path.write_text(script)
            path.chmod(0o755)

    def make(self, target, **env):
        return subprocess.run(
            ['make', '--no-print-directory', target, f'FREEDICT_TOOLS={REPO}',
             f'FREEDICTRC={self.root}/absent', f'XMLLINT={self.root}/validator',
             f'JING={self.root}/validator'], cwd=REPO,
            env=dict(self.env, **env), capture_output=True, text=True)

    def calls(self):
        return (self.root / 'calls').read_text()

    def test_generation_failure_cleans_up_and_skips_validation(self):
        self.assertNotEqual(self.make('api', API_STATUS='7').returncode, 0)
        self.assertIn('fd_file_mgr -u', self.calls())
        self.assertNotIn('validator', self.calls())

    def test_mount_failure_stops_generation(self):
        self.assertNotEqual(self.make('api', MOUNT_STATUS='8').returncode, 0)
        self.assertNotIn('fd_api', self.calls())

    def test_existing_mount_is_preserved(self):
        self.assertEqual(self.make('api', MOUNT_STATUS='201').returncode, 0)
        self.assertNotIn('fd_file_mgr -u', self.calls())
        self.assertIn(f'{self.root}/api dir/freedict-database.xml', self.calls())

    def test_path_failure_stops_validation(self):
        self.assertNotEqual(self.make('api-validation', PATH_STATUS='9').returncode, 0)
        self.assertNotIn('validator', self.calls())

    def test_both_validator_selections(self):
        for value in ['0', '1']:
            with self.subTest(value=value):
                (self.root / 'calls').write_text('')
                self.assertEqual(self.make('api-validation', USE_JING=value).returncode, 0)
                self.assertEqual('--relaxng' in self.calls(), value == '0')


    def test_release_path_failure_and_empty_path_propagate(self):
        for env in [{'PATH_STATUS': '9'}, {'API_DIR': ''}]:
            with self.subTest(env=env):
                self.assertNotEqual(self.make('release-path', **env).returncode, 0)
