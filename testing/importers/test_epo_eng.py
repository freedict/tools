"""Regression tests for the generated ESPDIC TEI."""

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


def import_entries(*lines):
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / 'espdic.txt'
        source.write_text('# ESPDIC (Esperanto – English Dictionary) – 6 October 2024'
                          ' - Paul Denisowski (www.denisowski.org)\n' + '\n'.join(lines),
                          encoding='utf-8-sig')
        (Path(tmp) / 'COPYING').touch()
        epo_eng.main(source, IMPORTER / 'header.tei', tmp)
        return ElementTree.parse(Path(tmp) / 'epo-eng.tei').findall('.//body/entry', NS)


class EntryTests(unittest.TestCase):
    def test_headword_punctuation_and_annotations(self):
        phrase, abbreviation, suffix = import_entries(
            'fino bona, ĉio bona : all’s well that ends well',
            'PGKD (plej granda komuna divizoro) : GCD (greatest common denominator)',
            '-a : (adjective ending)')
        self.assertEqual(phrase.findtext('form/orth', namespaces=NS), 'fino bona, ĉio bona')
        self.assertEqual(abbreviation.findtext('form/orth', namespaces=NS), 'PGKD')
        self.assertEqual(abbreviation.findtext('form/usg', namespaces=NS),
                         'plej granda komuna divizoro')
        self.assertEqual(suffix.find('form/orth', NS).get('extent'), 'suff')


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
