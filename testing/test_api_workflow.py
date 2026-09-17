"""API lifecycle checks without mounting or contacting remote services."""

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

from fd_tool.scripts import fd_api

REPO = Path(__file__).resolve().parents[1]


class ApiMakeTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.env = dict(os.environ, PATH=f'{self.root}{os.pathsep}{os.environ["PATH"]}',
                        REVIEW_LOG=str(self.root / 'calls'), API_DIR=str(self.root / 'api dir'))
        script = f'''#!{sys.executable}
import os, sys
from pathlib import Path
name = Path(sys.argv[0]).name
with open(os.environ['REVIEW_LOG'], 'a') as f:
    f.write(name + ' ' + ' '.join(sys.argv[1:]) + '\\n')
if name == 'fd_file_mgr':
    if sys.argv[1] == '-a':
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


class ApiPythonTests(unittest.TestCase):
    def test_shell_exit_code_is_not_wait_status(self):
        with self.assertRaises(SystemExit) as error:
            fd_api.exec_or_fail('exit 7')
        self.assertEqual(error.exception.code, 7)

    def test_output_directory_and_failure_cleanup(self):
        with tempfile.TemporaryDirectory() as tmp:
            destination = Path(tmp) / 'new' / 'api'
            conf = {'DEFAULT': {'api_output_path': str(destination)}}
            with patch.object(fd_api.config, 'discover_and_load', return_value=conf), \
                    patch.object(fd_api, 'read_dict_info', return_value=[]), \
                    patch.object(fd_api.releases, 'get_latest_tools_release', return_value={}), \
                    patch.object(fd_api.xmlhandlers, 'write_freedict_database', side_effect=ValueError), \
                    patch.object(fd_api, 'exec_or_fail') as execute, \
                    patch.object(fd_api.time, 'sleep'):
                with self.assertRaises(ValueError):
                    fd_api.main_body(['fd_api', '-o', 'cleanup'])
                self.assertTrue(destination.is_dir())
                execute.assert_called_with('cleanup')
