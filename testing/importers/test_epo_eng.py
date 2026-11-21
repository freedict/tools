"""Regression tests for ESPDIC attribution in the generated TEI."""

import importlib.util
import tempfile
import unittest
from pathlib import Path
from xml.etree import ElementTree

IMPORTER = Path(__file__).resolve().parents[2] / 'importers/epo-eng'
spec = importlib.util.spec_from_file_location('epo_eng', IMPORTER / 'epo-eng-import.py')
epo_eng = importlib.util.module_from_spec(spec)
spec.loader.exec_module(epo_eng)
NS = {'': 'http://www.tei-c.org/ns/1.0'}


class AttributionTests(unittest.TestCase):
    def test_import_writes_source_attribution(self):
        for date, author in [('6 October 2024', 'Paul Denisowski'),
                             ('1 March 2027', 'Jones & Smith')]:
            with self.subTest(author=author), tempfile.TemporaryDirectory() as tmp:
                source = Path(tmp) / 'espdic.txt'
                source.write_text(
                    f'# ESPDIC (Esperanto – English Dictionary) – {date} - '
                    f'{author} (www.denisowski.org)\nabelo : bee\n',
                    encoding='utf-8-sig')
                (Path(tmp) / 'COPYING').touch()
                epo_eng.main(source, IMPORTER / 'header.tei', tmp)
                tei = ElementTree.parse(Path(tmp) / 'epo-eng.tei')
                self.assertEqual(tei.findtext('.//availability/p', namespaces=NS),
                                 f'Copyright (C) {date[-4:]} {author}.')
                self.assertEqual(tei.findtext('.//body/entry/form/orth', namespaces=NS), 'abelo')

    def test_malformed_attribution_reports_first_line(self):
        with self.assertRaisesRegex(ValueError, 'line 1'):
            epo_eng.parse_attribution('# ESPDIC (Esperanto – English Dictionary)')
