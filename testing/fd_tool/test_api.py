"""Python API lifecycle checks without contacting remote services."""

import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from fd_tool.scripts import fd_api


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
