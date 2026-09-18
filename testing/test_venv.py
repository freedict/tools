"""Bootstrap tests keep dependency installation isolated from the host."""

import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

REPO = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('create_venv', REPO / 'buildhelpers/create_venv.py')
bootstrap = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bootstrap)


class BootstrapTests(unittest.TestCase):
    def test_uses_selected_python_and_checkout_directory(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(bootstrap.subprocess, 'run') as run:
            run.return_value.returncode = 0
            destination = Path(tmp) / 'env with spaces'
            bootstrap.create_environment(destination, REPO)
            self.assertEqual(run.call_args_list[0].args[0],
                             [sys.executable, '-m', 'venv', str(destination)])
            install = run.call_args_list[2]
            self.assertEqual(install.kwargs['cwd'], REPO)
            self.assertEqual(install.args[0][:4], [str(destination / 'bin/python'), '-m', 'pip', 'install'])

    def test_missing_icu_has_actionable_error(self):
        with tempfile.TemporaryDirectory() as tmp, patch.dict(os.environ, {}, clear=True), \
                patch.object(bootstrap.subprocess, 'run') as run:
            run.side_effect = [subprocess.CompletedProcess([], 0),
                               subprocess.CompletedProcess([], 1), FileNotFoundError()]
            with self.assertRaisesRegex(RuntimeError, 'libicu-dev'):
                bootstrap.create_environment(Path(tmp) / 'env', REPO)

    def test_install_failure_does_not_run_check(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(bootstrap.subprocess, 'run') as run:
            run.side_effect = [subprocess.CompletedProcess([], 0),
                               subprocess.CompletedProcess([], 0),
                               subprocess.CalledProcessError(7, 'pip')]
            with self.assertRaises(subprocess.CalledProcessError):
                bootstrap.create_environment(Path(tmp) / 'env', REPO)
            self.assertEqual(run.call_count, 3)

    def test_refuses_unrelated_directory(self):
        with tempfile.TemporaryDirectory() as tmp:
            (Path(tmp) / 'important').write_text('keep')
            with self.assertRaisesRegex(RuntimeError, 'non-venv'):
                bootstrap.create_environment(Path(tmp), REPO)
