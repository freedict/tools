"""Regression tests for importer replacement and remote-environment cleanup."""

import importlib.util
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from fd_tool.scripts.fd_file_mgr import UnisonFileAccess

spec = importlib.util.spec_from_file_location('wikdict', Path(__file__).parents[1] / 'importers/wikdict/import_wikdict.py')
wikdict = importlib.util.module_from_spec(spec)
spec.loader.exec_module(wikdict)


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
            self.assertFalse((root / 'eng-deu.old').exists())
            self.assertEqual(list(root.glob('.eng-deu.*')), [])

    def test_force_import_matches_exact_name(self):
        links = ['eng-deu.tei', 'eng-deu-extra.tei', 'deu-eng.tei']
        self.assertEqual([link for link in links if link.rsplit('/', 1)[-1] == 'eng-deu.tei'], ['eng-deu.tei'])
