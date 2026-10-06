"""Regression tests for safe WikDict importer replacement."""

import importlib.util
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('wikdict', Path(__file__).parents[2] / 'importers/wikdict/import_wikdict.py')
wikdict = importlib.util.module_from_spec(spec)
spec.loader.exec_module(wikdict)


class SafetyTests(unittest.TestCase):
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

    def test_failed_publication_restores_existing_dictionary(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            old = root / 'eng-deu'
            old.mkdir()
            (old / 'keep').write_text('old')
            (old / 'ChangeLog').write_bytes(b'previous {history}\r\n')
            shared = root / 'shared'
            shared.mkdir()
            for name in ['freedict-dictionary.css', 'freedict-P5.dtd', 'INSTALL',
                         'freedict-P5.rng', 'freedict-P5.xml']:
                (shared / name).write_text('fixture')
            replace = os.replace

            def fail_publication(source, destination):
                if Path(source).parent.name.startswith('.eng-deu.'):
                    raise PermissionError('publication denied')
                replace(source, destination)

            previous = os.getcwd()
            try:
                os.chdir(root)
                with patch.object(wikdict, 'download', return_value='<TEI/>'), \
                        patch.object(wikdict, 'enough_headwords', return_value=True), \
                        patch.object(wikdict.os, 'replace', side_effect=fail_publication), \
                        self.assertRaises(PermissionError):
                    wikdict.import_dictionary([], 'https://example.org/eng-deu.tei',
                                              str(shared), True)
            finally:
                os.chdir(previous)
            self.assertEqual((old / 'keep').read_text(), 'old')
            self.assertEqual((old / 'ChangeLog').read_bytes(), b'previous {history}\r\n')
            self.assertFalse((root / 'eng-deu.old').exists())
            self.assertEqual(list(root.glob('.eng-deu.*')), [])

    def test_force_import_matches_exact_name(self):
        links = ['eng-deu.tei', 'eng-deu-extra.tei', 'deu-eng.tei']
        self.assertEqual([link for link in links if link.rsplit('/', 1)[-1] == 'eng-deu.tei'], ['eng-deu.tei'])


    def test_successful_replacement_preserves_history_verbatim(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            old = root / 'eng-deu'
            old.mkdir()
            history = b'Previous {dict} and {unknown} history\r\n\nLast line without newline'
            (old / 'ChangeLog').write_bytes(history)
            shared = root / 'shared'
            shared.mkdir()
            for name in ['freedict-dictionary.css', 'freedict-P5.dtd', 'INSTALL',
                         'freedict-P5.rng', 'freedict-P5.xml']:
                (shared / name).write_text('fixture')
            previous = os.getcwd()
            try:
                os.chdir(root)
                with patch.object(wikdict, 'download', return_value='<TEI/>'), \
                        patch.object(wikdict, 'enough_headwords', return_value=True):
                    wikdict.import_dictionary([], 'https://example.org/eng-deu.tei',
                                              str(shared), True)
            finally:
                os.chdir(previous)
            result = (old / 'ChangeLog').read_bytes()
            self.assertTrue(result.endswith(history))
            self.assertIn(b'automatic import of eng-deu dictionary', result)
            self.assertNotIn(b'.eng-deu.', result)
            self.assertEqual(list(root.glob('.eng-deu.*')), [])

    def test_missing_history_creates_new_entry(self):
        with tempfile.TemporaryDirectory() as tmp:
            dictionary = Path(tmp) / 'eng-deu'
            dictionary.mkdir()
            wikdict.make_changelog(str(dictionary))
            first = (dictionary / 'ChangeLog').read_bytes()
            self.assertIn(b'automatic import of eng-deu dictionary', first)
            wikdict.make_changelog(str(dictionary))
            self.assertTrue((dictionary / 'ChangeLog').read_bytes().endswith(first))

    def test_history_read_failure_preserves_original_dictionary(self):
        with tempfile.TemporaryDirectory() as tmp:
            old = Path(tmp) / 'eng-deu'
            old.mkdir()
            (old / 'ChangeLog').write_bytes(b'original history')
            staged = Path(tmp) / 'staged'
            staged.mkdir()
            with patch('builtins.open', side_effect=PermissionError('history unreadable')), \
                    self.assertRaises(PermissionError):
                wikdict.make_changelog(str(staged), str(old))
            self.assertEqual((old / 'ChangeLog').read_bytes(), b'original history')
            self.assertEqual(list(staged.iterdir()), [])
