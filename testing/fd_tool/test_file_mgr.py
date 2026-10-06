"""Regression tests for Unison remote-environment cleanup."""

import os
import subprocess
import unittest
from unittest.mock import patch

from fd_tool.scripts.fd_file_mgr import UnisonFileAccess


class SafetyTests(unittest.TestCase):
    def test_unison_restores_environment_on_failure(self):
        for environment in [{}, {'UNISON': ''}, {'UNISON': 'original'}]:
            with self.subTest(environment=environment), \
                    patch.dict(os.environ, environment, clear=True), \
                    patch('fd_tool.scripts.fd_file_mgr.subprocess.run',
                          return_value=subprocess.CompletedProcess([], 1)):
                with self.assertRaises(OSError):
                    UnisonFileAccess().make_available('user', 'host', 'remote', '/tmp/local')
                self.assertEqual(dict(os.environ), environment)

    def test_unison_passes_paths_and_environment_to_child(self):
        with patch.dict(os.environ, {'UNISON': 'original'}, clear=True), \
                patch('fd_tool.scripts.fd_file_mgr.subprocess.run',
                      return_value=subprocess.CompletedProcess([], 0)) as run:
            UnisonFileAccess().make_available('user', 'host', 'remote path', '/tmp/local path')
            self.assertEqual(run.call_args.args[0][-2:],
                             ['ssh://user@host/remote path/', '/tmp/local path'])
            self.assertEqual(run.call_args.kwargs['env']['UNISON'], '/tmp/local path/.unison')
            self.assertEqual(os.environ['UNISON'], 'original')

    def test_unison_startup_failure_preserves_environment(self):
        with patch.dict(os.environ, {'UNISON': 'original'}, clear=True), \
                patch('fd_tool.scripts.fd_file_mgr.subprocess.run',
                      side_effect=FileNotFoundError('unison')):
            with self.assertRaises(FileNotFoundError):
                UnisonFileAccess().make_available('user', 'host', 'remote', '/tmp/local')
            self.assertEqual(os.environ['UNISON'], 'original')
