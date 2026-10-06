"""Tools archives must match the selected tag, not the working tree."""

import os
import shutil
import subprocess
import sys
import tarfile
import tempfile
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]


class ToolsReleaseTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.checkout = self.root / 'checkout'
        self.checkout.mkdir()
        for name in ['Makefile', 'mk/config.mk', 'buildhelpers/release.py']:
            destination = self.checkout / name
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(REPO / name, destination)
        (self.checkout / 'README').write_text('tagged content\n')
        (self.checkout / '.gitignore').write_text('*.cache\n')
        self.git('init', '-q')
        self.git('add', '.')
        self.git('-c', 'user.name=Test', '-c', 'user.email=test@example.org',
                 'commit', '-qm', 'Fixture')
        self.git('tag', '0.8.0')
        self.output = self.root / 'output with spaces'

    def git(self, *arguments):
        return subprocess.check_output(['git', '-C', str(self.checkout), *arguments], text=True)

    def make(self, *arguments, **env):
        return subprocess.run(
            ['make', '--no-print-directory', 'release', f'FREEDICT_TOOLS={self.checkout}',
             f'PYTHON={sys.executable}', f'BUILD_DIR={self.output}', *arguments],
            cwd=self.checkout, env={**os.environ, 'TAG': '', **env},
            capture_output=True, text=True, check=False)

    def archive(self):
        return self.output / 'freedict-tools-0.8.0.tar.xz'

    def test_only_tagged_files_are_archived(self):
        (self.checkout / 'README').write_text('later commit\n')
        self.git('add', 'README')
        self.git('-c', 'user.name=Test', '-c', 'user.email=test@example.org',
                 'commit', '-qm', 'Newer commit')
        (self.checkout / 'README').write_text('uncommitted change\n')
        (self.checkout / 'untracked').write_text('do not include')
        (self.checkout / 'ignored.cache').write_text('do not include')
        result = self.make('TAG=0.8.0')
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('Warning:', result.stderr)
        with tarfile.open(self.archive(), 'r:xz') as archive:
            self.assertEqual(archive.extractfile('tools/README').read(), b'tagged content\n')
            names = archive.getnames()
            self.assertIn('tools/.gitignore', names)
            self.assertNotIn('tools/untracked', names)
            self.assertNotIn('tools/ignored.cache', names)
            self.assertNotIn('tools/.git', names)

    def test_missing_tag_branch_and_commit_are_rejected(self):
        for arguments in [[], ['TAG=missing'], ['TAG=' + self.git('branch', '--show-current').strip()],
                          ['TAG=' + self.git('rev-parse', 'HEAD').strip()]]:
            with self.subTest(arguments=arguments):
                self.assertNotEqual(self.make(*arguments).returncode, 0)
                self.assertFalse(self.output.exists())

    def test_annotated_tag_and_matching_head(self):
        self.git('-c', 'user.name=Test', '-c', 'user.email=test@example.org',
                 'tag', '-a', '0.8.1', '-m', 'Annotated fixture tag')
        result = self.make('TAG=0.8.1')
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotIn('Warning:', result.stderr)
        with tarfile.open(self.output / 'freedict-tools-0.8.1.tar.xz', 'r:xz') as archive:
            self.assertEqual(archive.extractfile('tools/README').read(), b'tagged content\n')

    def test_existing_artifact_is_regenerated(self):
        self.output.mkdir()
        self.archive().write_bytes(b'stale archive')
        self.assertEqual(self.make('TAG=0.8.0').returncode, 0)
        with tarfile.open(self.archive(), 'r:xz') as archive:
            self.assertIn('tools/README', archive.getnames())

    def test_export_failure_preserves_existing_artifact(self):
        self.output.mkdir()
        self.archive().write_bytes(b'previous archive')
        bin_dir = self.root / 'bin'
        bin_dir.mkdir()
        git = bin_dir / 'git'
        git.write_text(f'''#!{sys.executable}
import os, sys
if 'archive' in sys.argv:
    sys.stdout.buffer.write(b'partial archive')
    sys.exit(7)
os.execv({shutil.which('git')!r}, ['git', *sys.argv[1:]])
''')
        git.chmod(0o755)
        result = self.make('TAG=0.8.0', PATH=str(bin_dir) + os.pathsep + os.environ['PATH'])
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.archive().read_bytes(), b'previous archive')
        self.assertEqual(list(self.output.iterdir()), [self.archive()])
