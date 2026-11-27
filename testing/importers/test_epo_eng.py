"""Regression tests for the generated ESPDIC TEI."""

import importlib.util
import shutil
import tempfile
import unittest
from datetime import date
from pathlib import Path
from unittest.mock import patch
from xml.etree import ElementTree

IMPORTER = Path(__file__).resolve().parents[2] / 'importers/epo-eng'
spec = importlib.util.spec_from_file_location('epo_eng', IMPORTER / 'epo-eng-import.py')
epo_eng = importlib.util.module_from_spec(spec)
spec.loader.exec_module(epo_eng)
NS = {'': 'http://www.tei-c.org/ns/1.0'}


def make_shared_dir(root):
    shared = root / 'shared'
    shared.mkdir()
    for name in ('freedict-P5.dtd', 'freedict-P5.rng', 'freedict-P5.xml', 'freedict-dictionary.css'):
        (shared / name).write_bytes(name.encode())
    return shared


def import_entries(*lines):
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / 'espdic.txt'
        shutil.copy(IMPORTER / 'epo-eng.patch', tmp)
        source.write_text('# ESPDIC (Esperanto – English Dictionary) – 6 October 2024'
                          ' - Paul Denisowski (www.denisowski.org)\n' + '\n'.join(lines),
                          encoding='utf-8-sig')
        (Path(tmp) / 'COPYING').touch()
        skeleton = shutil.copy(IMPORTER / 'header.tei', tmp)
        epo_eng.main(source, skeleton, tmp, make_shared_dir(Path(tmp)))
        return ElementTree.parse(Path(tmp) / 'epo-eng.tei').findall('.//body/entry', NS)


class EntryTests(unittest.TestCase):
    def test_medial_parentheses_stay_inside_the_quote(self):
        optional, alternative, prefix = import_entries(
            'alformiĝi en : to transform (oneself) into',
            'apenaŭ videbla : hardly (barely) visible',
            'ĉe la reto : on the (inter)net')
        self.assertEqual(optional.findtext('sense/cit/quote', namespaces=NS),
                         'transform (oneself) into')
        self.assertEqual(alternative.findtext('sense/cit/quote', namespaces=NS),
                         'hardly (barely) visible')
        self.assertEqual(prefix.findtext('sense/cit/quote', namespaces=NS), 'on the (inter)net')

    def test_medial_groups_stop_before_qualifier_or_delimiter(self):
        entry, = import_entries('provi : a (b) (c) d (e) f (hint), g; h')
        first, second = entry.findall('sense', NS)
        self.assertEqual([q.text for q in first.findall('cit/quote', NS)],
                         ['a (b) (c) d (e) f', 'g'])
        self.assertEqual(first.findtext('cit/usg', namespaces=NS), 'hint')
        self.assertEqual(second.findtext('cit/quote', namespaces=NS), 'h')

    def test_prepositional_to_is_part_of_the_translation(self):
        phrases = [('ĝisfine', 'to the end'), ('certagrade', 'to a certain degree'),
                   ('al si reciproke', 'to one another')]
        entries = import_entries(*(f'{head} : {quote}' for head, quote in phrases))
        for entry, (_, quote) in zip(entries, phrases, strict=True):
            self.assertIsNone(entry.find('gramGrp/pos', NS))
            self.assertEqual(entry.findtext('sense/cit/quote', namespaces=NS), quote)

    def test_later_equivalents_keep_prepositional_to(self):
        entry, = import_entries('provi : to attempt, to the end')
        self.assertEqual(entry.findtext('gramGrp/pos', namespaces=NS), 'v')
        self.assertEqual([q.text for q in entry.findall('sense/cit/quote', NS)],
                         ['attempt', 'to the end'])

    def test_to_collocation_stays_with_its_translation(self):
        verb, preposition = import_entries('aspiri : to aspire (to), hope for',
                                           'apud : at, beside, by, near (to), nearby, next to')
        self.assertEqual(verb.findtext('gramGrp/pos', namespaces=NS), 'v')
        self.assertIsNone(preposition.find('gramGrp/pos', NS))
        for entry, expected in [(verb, 'aspire'), (preposition, 'near')]:
            qualified = [cit for cit in entry.findall('sense/cit', NS)
                         if cit.findtext('gramGrp/colloc', namespaces=NS) == 'to']
            self.assertEqual([cit.findtext('quote', namespaces=NS) for cit in qualified], [expected])

    def test_literal_brackets_are_separate_translations(self):
        for symbol in ('{', '}', '{}', '[', ']', '[]', '(', ')', '()'):
            with self.subTest(symbol=symbol):
                entry, suffix = import_entries(f'krampo : bracket, {symbol}; support',
                                               '-a : (adjective ending)')
                translations, support = entry.findall('sense', NS)
                self.assertEqual([q.text for q in translations.findall('cit/quote', NS)],
                                 ['bracket', symbol])
                self.assertEqual(support.findtext('cit/quote', namespaces=NS), 'support')
                self.assertEqual(suffix.findtext('sense/cit/def', namespaces=NS), 'adjective ending')

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


class ImportTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.shared = make_shared_dir(self.root)
        self.header = Path(shutil.copy(IMPORTER / 'header.tei', self.root))
        input_dir = self.root / 'input'
        input_dir.mkdir()
        self.source = input_dir / 'espdic.txt'
        shutil.copy(IMPORTER / 'epo-eng.patch', input_dir)
        (self.root / 'COPYING').touch()

    def run_import(self, release, holder='Paul Denisowski'):
        self.source.write_text(
            f'# ESPDIC (Esperanto – English Dictionary) – {release} - '
            f'{holder} (www.denisowski.org)\nabelo : bee\n# comment\n',
            encoding='utf-8-sig')
        epo_eng.main(self.source, self.header, self.root, self.shared)
        return ElementTree.parse(self.root / 'epo-eng.tei')

    def test_shared_files_are_copied_and_refreshed(self):
        self.run_import('6 October 2024')
        for source in self.shared.iterdir():
            self.assertEqual((self.root / source.name).read_bytes(), source.read_bytes())
            source.write_bytes(b'updated ' + source.read_bytes())
        self.run_import('6 October 2024')
        for source in self.shared.iterdir():
            self.assertEqual((self.root / source.name).read_bytes(), source.read_bytes())

    def test_patch_is_copied_from_beside_the_input(self):
        source_patch = self.source.with_name('epo-eng.patch')
        for contents in (b'first patch\r\n', b'revised patch\r\n'):
            source_patch.write_bytes(contents)
            self.run_import('6 October 2024')
            self.assertEqual((self.root / 'epo-eng.patch').read_bytes(), contents)

    def test_missing_shared_file_fails_before_writing_output(self):
        missing = self.shared / 'freedict-P5.rng'
        missing.unlink()
        with self.assertRaises(FileNotFoundError) as error:
            self.run_import('6 October 2024')
        self.assertIn(str(missing), str(error.exception))
        self.assertFalse((self.root / 'epo-eng.tei').exists())

    def test_existing_release_updates_both_headers_without_adding_history(self):
        # Exercise the skeleton format used before date-only editions.
        tree = ElementTree.parse(self.header)
        edition = tree.findtext('.//edition', namespaces=NS)
        self.header.write_text(self.header.read_text().replace(
            f'<edition>{edition}</edition>', '<edition>2024.10.06+fd1</edition>'))
        changes = tree.findall('.//change', NS)
        output = self.run_import('6 October 2024')
        updated = ElementTree.parse(self.header)
        for doc in [updated, output]:
            self.assertEqual(doc.findtext('.//edition', namespaces=NS), '2024.10.06')
            self.assertEqual(doc.findtext('.//fileDesc/extent', namespaces=NS), '1 headword')
            published = doc.find('.//publicationStmt/date', NS)
            self.assertEqual((published.text, published.get('when')), (date.today().isoformat(),) * 2)
            self.assertEqual(doc.findtext('.//availability/p', namespaces=NS),
                             'Copyright (C) 2024 Paul Denisowski.')
            self.assertEqual([e.attrib for e in doc.findall('.//change', NS)],
                             [e.attrib for e in changes])
        self.assertEqual(len(updated.findall('.//body/entry', NS)), 0)
        self.assertEqual(len(output.findall('.//body/entry', NS)), 1)

    def test_new_release_recorded_once(self):
        before = ElementTree.parse(self.header).findall('.//change', NS)
        with patch.object(epo_eng.getpass, 'getuser', return_value='tester'):
            self.run_import('1 March 2027', 'Jones & Smith')
            saved = self.header.read_bytes()
            output = self.run_import('1 March 2027', 'Jones & Smith')
        self.assertEqual(self.header.read_bytes(), saved)
        for doc in [ElementTree.parse(self.header), output]:
            changes = doc.findall('.//revisionDesc/change', NS)
            self.assertEqual(len(changes), len(before) + 1)
            self.assertEqual(doc.findtext('.//edition', namespaces=NS), '2027.03.01')
            self.assertEqual(doc.findtext('.//availability/p', namespaces=NS),
                             'Copyright (C) 2027 Jones & Smith.')
            self.assertEqual(changes[0].attrib, {'n': '2027.03.01', 'when': date.today().isoformat()})
            self.assertEqual(changes[0].findtext('name', namespaces=NS), 'tester')
            self.assertIn('Import new ESPDIC version dated 2027-03-01.', ''.join(changes[0].itertext()))

    def test_recognised_contributor_uses_who(self):
        with patch.object(epo_eng.getpass, 'getuser', return_value='humenda'):
            output = self.run_import('2 March 2027')
        self.assertEqual(output.find('.//revisionDesc/change', NS).get('who'), '#humenda')

    def test_legacy_version_in_change_identifier(self):
        self.header.write_text(self.header.read_text().replace(
            '<revisionDesc>', '<revisionDesc><change n="2027.03.02+fd1">First import.</change>'))
        with patch.object(epo_eng.getpass, 'getuser', side_effect=AssertionError):
            output = self.run_import('2 March 2027')
        self.assertEqual(output.find('.//revisionDesc/change', NS).get('n'), '2027.03.02+fd1')

    def test_output_failure_preserves_skeleton(self):
        saved = self.header.read_bytes()
        with patch.object(epo_eng.fd_import.output, 'mk_makefile', side_effect=OSError), self.assertRaises(OSError):
            self.run_import('1 March 2027')
        self.assertEqual(self.header.read_bytes(), saved)

    def test_malformed_attribution_reports_first_line(self):
        with self.assertRaisesRegex(ValueError, 'line 1'):
            epo_eng.parse_attribution('# ESPDIC (Esperanto – English Dictionary)')
