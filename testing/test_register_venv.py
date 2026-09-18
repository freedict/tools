"""Configuration registration regression tests."""

import configparser
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
            config.write_text('[DEFAULT]\napi_output_path = /tmp/api\n[release]\nuser = test\n')
            self.assertTrue(register_venv.register(config, Path(tmp) / 'venv', 'yes'))
            self.assertTrue(register_venv.register(config, Path(tmp) / 'venv', 'yes'))
            parser = configparser.ConfigParser(); parser.read(config)
            self.assertEqual(parser['DEFAULT']['api_output_path'], '/tmp/api')
            self.assertEqual(parser['DEFAULT']['virtual_env'], str((Path(tmp) / 'venv').resolve()))
            self.assertEqual(parser['release']['user'], 'test')

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
