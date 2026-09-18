"""Configuration registration regression tests."""

import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('register_venv', Path(__file__).parents[1] / 'buildhelpers/register_venv.py')
register_venv = importlib.util.module_from_spec(spec)
spec.loader.exec_module(register_venv)


class RegisterTests(unittest.TestCase):
    def test_preserves_existing_settings_and_is_idempotent(self):
        with tempfile.TemporaryDirectory() as tmp:
            config = Path(tmp) / 'freedictrc'
            config.write_text('# keep this comment\n[DEFAULT]\napi_output_path = /tmp/api\n[release]\nuser = test\n')
            self.assertTrue(register_venv.register(config, Path(tmp) / 'venv', 'yes'))
            self.assertTrue(register_venv.register(config, Path(tmp) / 'venv', 'yes'))
            result = config.read_text()
            self.assertIn('# keep this comment\n', result)
            self.assertIn('api_output_path = /tmp/api\n', result)
            self.assertIn(f'virtual_env = {(Path(tmp) / "venv").resolve()}\n', result)
            self.assertIn('[release]\nuser = test\n', result)

    def test_noninteractive_auto_skips(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(register_venv.sys.stdin, 'isatty', return_value=False):
            config = Path(tmp) / 'freedictrc'
            self.assertFalse(register_venv.register(config, Path(tmp) / 'venv'))
            self.assertFalse(config.exists())

    def test_no_never_writes(self):
        with tempfile.TemporaryDirectory() as tmp:
            config = Path(tmp) / 'freedictrc'
            self.assertFalse(register_venv.register(config, Path(tmp) / 'venv', 'no'))
            self.assertFalse(config.exists())
