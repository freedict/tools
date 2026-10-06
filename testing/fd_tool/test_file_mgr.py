"""Regression tests for Unison remote-environment cleanup."""

import configparser
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from fd_tool.scripts import fd_file_mgr
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


class RemoteSessionTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.conf = configparser.ConfigParser()
        self.conf['DEFAULT'] = {'file_access_via': 'sshfs'}
        for section in ['release', 'generated']:
            path = Path(self.tmp.name, section)
            path.mkdir()
            self.conf[section] = {'user': 'user', 'server': 'host',
                                  'remote_path': 'remote path', 'local_path': str(path),
                                  'skip': 'no'}

    def run_command(self, acquisitions, child=0, cleanup=None):
        with patch.object(fd_file_mgr.SshfsAccess, 'make_available',
                          side_effect=acquisitions), \
                patch.object(fd_file_mgr.SshfsAccess, 'make_unavailable',
                             side_effect=cleanup) as release, \
                patch.object(fd_file_mgr.subprocess, 'run',
                             return_value=subprocess.CompletedProcess([], child)) as run:
            status = fd_file_mgr.run_with_files(self.conf, ['command', 'arg with spaces'])
            self.assertEqual(run.call_args.args[0], ['command', 'arg with spaces'])
            return status, release.call_args_list

    def test_mixed_mounts_preserve_existing_mount(self):
        for acquisitions, owned in [([None, 201], 'release'), ([201, None], 'generated')]:
            with self.subTest(acquisitions=acquisitions):
                status, calls = self.run_command(acquisitions)
                self.assertEqual(status, 0)
                self.assertEqual([call.args[0] for call in calls],
                                 [self.conf[owned]['local_path']])

    def test_all_existing_mounts_are_preserved(self):
        status, calls = self.run_command([201, 201])
        self.assertEqual((status, calls), (0, []))

    def test_child_failure_still_cleans_up_in_reverse_order(self):
        status, calls = self.run_command([None, None], child=7)
        self.assertEqual(status, 7)
        self.assertEqual([call.args[0] for call in calls],
                         [self.conf[name]['local_path'] for name in ['generated', 'release']])

    def test_cleanup_failure_does_not_mask_child_failure(self):
        for child, expected in [(0, 9), (7, 7)]:
            with self.subTest(child=child):
                status, calls = self.run_command(
                    [None, None], child=child,
                    cleanup=[fd_file_mgr.FileAccessError('cleanup failed', 9), None])
                self.assertEqual(status, expected)
                self.assertEqual(len(calls), 2)

    def test_setup_failure_rolls_back_only_new_mounts(self):
        for first, expected in [(None, 1), (201, 0)]:
            with self.subTest(first=first), \
                    patch.object(fd_file_mgr.SshfsAccess, 'make_available',
                                 side_effect=[first, fd_file_mgr.FileAccessError('mount failed', 8)]), \
                    patch.object(fd_file_mgr.SshfsAccess, 'make_unavailable') as release, \
                    patch.object(fd_file_mgr.subprocess, 'run') as run:
                with self.assertRaises(fd_file_mgr.FileAccessError) as error:
                    fd_file_mgr.run_with_files(self.conf, ['command'])
                self.assertEqual(error.exception.returncode, 8)
                self.assertEqual(release.call_count, expected)
                run.assert_not_called()

    def test_command_startup_failure_cleans_up(self):
        with patch.object(fd_file_mgr.SshfsAccess, 'make_available', return_value=None), \
                patch.object(fd_file_mgr.SshfsAccess, 'make_unavailable') as release, \
                patch.object(fd_file_mgr.subprocess, 'run', side_effect=FileNotFoundError):
            self.assertEqual(fd_file_mgr.run_with_files(self.conf, ['missing']), 1)
            self.assertEqual(release.call_count, 2)

    def test_interrupt_cleans_up(self):
        with patch.object(fd_file_mgr.SshfsAccess, 'make_available', return_value=None), \
                patch.object(fd_file_mgr.SshfsAccess, 'make_unavailable') as release, \
                patch.object(fd_file_mgr.subprocess, 'run', side_effect=KeyboardInterrupt):
            with self.assertRaises(KeyboardInterrupt):
                fd_file_mgr.run_with_files(self.conf, ['command'])
            self.assertEqual(release.call_count, 2)

    def test_unison_upload_uses_each_sections_arguments(self):
        self.conf['DEFAULT']['file_access_via'] = 'unison'
        with patch.object(fd_file_mgr.subprocess, 'run',
                          return_value=subprocess.CompletedProcess([], 0)) as run:
            self.assertEqual(fd_file_mgr.run_with_files(self.conf, ['command']), 0)
        self.assertEqual([call.args[0][-1] for call in run.call_args_list],
                         [self.conf['release']['local_path'], self.conf['generated']['local_path'],
                          'command', self.conf['generated']['local_path'], self.conf['release']['local_path']])

    def test_standalone_unison_cleanup_reconstructs_arguments(self):
        self.conf['DEFAULT']['file_access_via'] = 'unison'
        with patch.object(fd_file_mgr.subprocess, 'run',
                          return_value=subprocess.CompletedProcess([], 0)) as run:
            self.assertEqual(fd_file_mgr.cleanup(fd_file_mgr.create_access_sessions(self.conf)), 0)
        self.assertEqual([call.args[0][-1] for call in run.call_args_list],
                         [self.conf[name]['local_path'] for name in ['generated', 'release']])

    def test_standalone_mount_status_tracks_all_sections(self):
        for acquisitions, expected in [([None, 201], 0), ([201, None], 0), ([201, 201], 201)]:
            with self.subTest(acquisitions=acquisitions), \
                    patch.object(fd_file_mgr.config, 'discover_and_load', return_value=self.conf), \
                    patch.object(fd_file_mgr.SshfsAccess, 'make_available', side_effect=acquisitions), \
                    patch.object(fd_file_mgr.sys, 'argv', ['fd_file_mgr', '-m']):
                with self.assertRaises(SystemExit) as error:
                    fd_file_mgr.main()
                self.assertEqual(error.exception.code, expected)

    def test_sshfs_passes_paths_without_shell_expansion(self):
        with patch.object(fd_file_mgr.os.path, 'ismount', return_value=False), \
                patch.object(fd_file_mgr.os, 'listdir', return_value=[]), \
                patch.object(fd_file_mgr.subprocess, 'run',
                             return_value=subprocess.CompletedProcess([], 0, '', '')) as run:
            fd_file_mgr.SshfsAccess().make_available('user', 'host', 'remote path', '/tmp/local path')
            self.assertEqual(run.call_args.args[0], ['sshfs', 'user@host:remote path', '/tmp/local path'])
