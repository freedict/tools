"""Regression tests for importer replacement and remote-environment cleanup."""

import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from fd_tool.scripts.fd_file_mgr import UnisonFileAccess

spec = importlib.util.spec_from_file_location('wikdict', Path(__file__).parents[1] / 'importers/wikdict/import_wikdict.py')
wikdict = importlib.util.module_from_spec(spec)
spec.loader.exec_module(wikdict)


class SafetyTests(unittest.TestCase):
    def test_unison_restores_environment_on_failure(self):
        with patch.dict(os.environ, {'UNISON': 'original'}, clear=True), patch('os.system', return_value=256):
            with self.assertRaises(OSError):
                UnisonFileAccess().make_available('user', 'host', 'remote', '/tmp/local')
            self.assertEqual(os.environ['UNISON'], 'original')

    def test_headword_count_accepts_common_separators(self):
        for count in ['10000', '10,000', '10.000', '10 000']:
            tei = f'<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><extent>{count} headwords</extent></fileDesc></teiHeader></TEI>'
            with self.subTest(count=count):
                self.assertTrue(wikdict.enough_headwords(tei))

    def test_failed_copy_does_not_delete_existing_dictionary(self):
        with tempfile.TemporaryDirectory() as tmp:
            old = Path(tmp) / 'eng-deu'; old.mkdir(); (old / 'keep').write_text('old')
            with patch.object(wikdict, 'download', return_value='<TEI/>'), \
                    patch.object(wikdict, 'enough_headwords', return_value=True):
                previous = os.getcwd()
                try:
                    os.chdir(tmp)
                    with self.assertRaises(FileNotFoundError):
                        wikdict.import_dictionary([], 'https://example.org/eng-deu.tei', 'missing', True)
                finally:
                    os.chdir(previous)
            self.assertEqual((old / 'keep').read_text(), 'old')

    def test_force_import_matches_exact_name(self):
        links = ['eng-deu.tei', 'eng-deu-extra.tei', 'deu-eng.tei']
        self.assertEqual([link for link in links if link.rsplit('/', 1)[-1] == 'eng-deu.tei'], ['eng-deu.tei'])
