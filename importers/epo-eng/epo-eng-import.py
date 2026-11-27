#!/usr/bin/env python3

"""This file imports the epo-eng dictionary from
<http://www.denisowski.org/Esperanto/ESPDIC/espdic_readme.htm>. Please see the
README for more information."""

import enum
import getpass
import os
import re
import shutil
import sys
import tempfile
import urllib.request
from datetime import date
from pathlib import Path
from xml.dom import minidom
from xml.sax import saxutils

import fd_import
from fd_import.tokenizer import ChunkType, tokenize

SHARED_FILES = ('freedict-P5.dtd', 'freedict-P5.rng', 'freedict-dictionary.css')


class WordType(enum.Enum):
    Full = '' # normal words
    Prefix = 'pref' # word prefixes
    Suffix = 'suff' # :)
    Part = 'part' # "-x-", so an infix

class Definition:
    #pylint: disable=too-few-public-methods
    """Only relevant for headwords with no direct translations, they define the
    headword."""
    def __init__(self, definition):
        self.definition = definition

    def as_xml(self):
        return f'<cit>\n<def>{saxutils.escape(self.definition)}</def>\n</cit>'

class Word:
    #pylint: disable=too-few-public-methods
    """This is either a head word or a translation."""
    def __init__(self, word, usage_info=None):
        self.word = word
        # gramGrp is a list of ready-to-use xml strings, basically *one* node
        self.gramGrp, self.usage_info = self.__get_gram_info(usage_info)
        self.word_type = type

    def __repr__(self):
        return f'{self.word} ({self.usage_info})' if self.usage_info else self.word

    def __get_gram_info(self, usage_info):
        """Figure out, whether text is a collocating word or a usage hint.
        Return tuple ready-to-use XML. """
        # special cases: catch collocations
        collocations = ['for', 'of', 'up', 'to', 'in', 'on', 'by', 'upon',
            'ke', 'de', "do"] # esperanto
        if not usage_info:
            return (None, None)
        elif usage_info in collocations:
            return ([f'<colloc>{saxutils.escape(usage_info)}</colloc>'], None)
        else:
            return (None, f'<usg type="hint">{saxutils.escape(usage_info)}</usg>')

    def as_xml(self):
        xml = ['<cit type="trans">\n<quote>', saxutils.escape(self.word),
                '</quote>']
        if self.usage_info:
            xml.append('\n' + self.usage_info) # already escaped
        if self.gramGrp:
            grammar = '\n'.join(self.gramGrp)
            xml.append(f'\n<gramGrp>\n{grammar}\n</gramGrp>')
        return ''.join(xml + ['\n</cit>'])

#pylint: disable=redefined-variable-type
class HeadWord(Word):
    #pylint: disable=too-few-public-methods
    def __init__(self, word, usg=None):
        if word.startswith('-') and word.endswith('-'):
            self.type = WordType.Part
        elif word.startswith('-'):
            self.type = WordType.Suffix
        elif word.endswith('-'):
            self.type = WordType.Prefix
        else:
            self.type = WordType.Full
        self.word = word.lstrip('-').rstrip('-')
        super().__init__(self.word, usg)

    def as_xml(self):
        extent = ''
        if self.type != WordType.Full:
            extent = f' extent="{self.type.value}"'
        orth = f'<orth{extent}>{saxutils.escape(self.word)}</orth>'
        if self.gramGrp:
            grammar = '\n'.join(self.gramGrp)
            orth += f'\n<gramGrp>\n{grammar}</gramGrp>'
        if self.usage_info:
            orth += '\n' + self.usage_info
        return f'<form>\n{orth}\n</form>'

def structure_translations(unordered_list):
    """Take a list of chunks (see docs of parse_meanings) and return a list of
    list of words. The outer lists holds different meanings (homonyms) and has
    in most cases only one entry. The inner list contains objects of type
    Word."""
    translations = [[]] # initialize list with one sense
    while unordered_list:
        chunk = unordered_list.pop(0)
        # test for type of chunk
        if chunk[0] == ChunkType.Paren:
            # is this an additional info for a word:
            if unordered_list and unordered_list[0][0] == ChunkType.Word:
                translations[-1].append(Word(unordered_list.pop(0)[1], chunk[1]))
            else: # no next chunk or no word
                # this is either a parenthesized expression with no word before
                # or after, then it's a definition; otherwise it's an unhandled
                # case and a bug
                if (len(translations) == 1 and not translations[0]) and \
                        (not unordered_list or unordered_list[0][0] == ChunkType.Comma): # definition
                    translations[-1].append(Definition(chunk[1]))
                else:
                    print(',',repr(translations))
                    raise ValueError(
                        f"Couldn't parse translations; tokens parsed: {translations}\n"
                        f"Current token: {chunk}\nTokens left: {unordered_list}"
                    )
        elif chunk[0] == ChunkType.Word:
            qualifier = None
            if unordered_list and unordered_list[0][0] == ChunkType.Paren:
                qualifier = unordered_list.pop(0)[1]
            translations[-1].append(Word(chunk[1], qualifier))
        elif chunk[0] == ChunkType.Comma: # new word starts, ignore
            pass
        elif chunk[0] == ChunkType.Semicolon: # homonym, new sense
            translations.append([])
        # add brackets and braces verbatim
        elif chunk[0] in (ChunkType.Bracket, ChunkType.Brace):
            chars = ('[]' if chunk[0] == ChunkType.Bracket else '{}')
            # readd translation in bracket/brace, verbatim
            if translations[-1]: # at least one word found
                translations[-1][-1].word += ' ' + chars[0] + chunk[1] + chars[1]
            else: # new word
                translations[-1].append(Word(chars[0] + chunk[1] + chars[1]))
        else:
            raise NotImplementedError("Unhandled case: " + repr(chunk))
    return translations

def looks_like_infinitive(text):
    """Exclude common prepositional phrases such as 'to the end'."""
    return text.startswith('to ') and not re.match(
        r'to\s+(?:the|a|an|my|what|some|both|one\s+another)\b', text)


def guess_grammar_details(translations):
    """Infer verb status from the first equivalent and strip infinitive markers."""
    gram = ''
    firstword = translations[0][0]
    if isinstance(firstword, Definition):
        return (None, translations) # don't fiddle around with def's
    if looks_like_infinitive(firstword.word):
        gram = '<pos>v</pos>'
        for deflist in translations:
            for word in deflist:
                if isinstance(word, Word) and looks_like_infinitive(word.word):
                    word.word = word.word.removeprefix('to ')
    return (gram, translations)


def translations_to_xml(translations):
    """Translate nested definition / translation (word) structure into a
    TEI:sense structure."""
    xml = []
    for definition_list in translations:
        if not definition_list:
            raise ValueError("Found empty translation list; all translations: " \
                    + repr(translations))
        xml.append('<sense>')
        xml.extend(x.as_xml() for x in definition_list)
        xml.append('</sense>')
    return xml

def parse_attribution(line):
    match = re.fullmatch(
        r"# ESPDIC \([^)]+\)\s+[-–]\s+(\d{1,2}) ([A-Za-z]+) (\d{4})"
        r"\s+[-–]\s+([^()]+?)\s+\([^)]+\)", line.strip())
    if not match:
        raise ValueError("line 1: expected the ESPDIC edition date and author")
    day, month, year, author = match.groups()
    months = ('January', 'February', 'March', 'April', 'May', 'June', 'July',
              'August', 'September', 'October', 'November', 'December')
    try:
        release = date(int(year), months.index(month) + 1, int(day))
    except ValueError as error:
        raise ValueError("line 1: invalid ESPDIC edition date") from error
    return release, author


def replace_text(element, value):
    while element.firstChild:
        element.removeChild(element.firstChild)
    element.appendChild(element.ownerDocument.createTextNode(value))


def element_text(node):
    return ''.join(child.data if child.nodeType in (child.TEXT_NODE, child.CDATA_SECTION_NODE)
                   else element_text(child) for child in node.childNodes)


def prepend_element(parent, element):
    """Use the existing first child's indentation for the new sibling."""
    first = parent.firstChild
    parent.insertBefore(element, first)
    if first and first.nodeType == first.TEXT_NODE and not first.data.strip():
        parent.insertBefore(first.cloneNode(False), element)


def record_release(skeleton, release, edition, today):
    history, = skeleton.getElementsByTagName('revisionDesc')
    dates = re.compile(rf'\b(?:{re.escape(edition)}|{release.isoformat()})\b')
    for change in history.getElementsByTagName('change'):
        if (change.getAttribute('n').partition('+')[0] in (edition, release.isoformat())
                or dates.search(element_text(change))):
            return
    change = skeleton.createElement('change')
    change.setAttribute('n', edition)
    change.setAttribute('when', today)
    username = getpass.getuser()
    title, = skeleton.getElementsByTagName('titleStmt')
    if any(name.getAttribute('xml:id') == username for name in title.getElementsByTagName('name')):
        change.setAttribute('who', '#' + username)
    else:
        name = skeleton.createElement('name')
        replace_text(name, username)
        change.appendChild(name)
        change.appendChild(skeleton.createTextNode(' '))
    change.appendChild(skeleton.createTextNode(f'Import new ESPDIC version dated {release}.'))
    prepend_element(history, change)


def updated_header(tei_skeleton, release, author, count):
    today = date.today().isoformat()
    edition = release.isoformat().replace('-', '.')
    with minidom.parse(os.fspath(tei_skeleton)) as skeleton:
        body, = skeleton.getElementsByTagName('body')
        if any(node.nodeType == node.ELEMENT_NODE for node in body.childNodes) or element_text(body).strip():
            raise ValueError("Expected a TEI skeleton with an empty body")
        if not body.hasChildNodes():
            body.appendChild(skeleton.createTextNode('\n'))
        description, = skeleton.getElementsByTagName('fileDesc')
        edition_stmt, = description.getElementsByTagName('editionStmt')
        edition_element, = edition_stmt.getElementsByTagName('edition')
        extent, = (node for node in description.childNodes if node.nodeName == 'extent')
        publication, = description.getElementsByTagName('publicationStmt')
        published, = (node for node in publication.childNodes if node.nodeName == 'date')
        replace_text(edition_element, edition)
        replace_text(extent, f"{count} {'headword' if count == 1 else 'headwords'}")
        replace_text(published, today)
        published.setAttribute('when', today)
        record_release(skeleton, release, edition, today)
        availability, = publication.getElementsByTagName('availability')
        notice = skeleton.createElement('p')
        notice.appendChild(skeleton.createTextNode(f'Copyright (C) {release.year} {author}.'))
        for paragraph in availability.childNodes:
            if (paragraph.nodeName == 'p' and paragraph.firstChild
                    and paragraph.firstChild.nodeType == paragraph.TEXT_NODE
                    and paragraph.firstChild.data.lstrip().startswith('Copyright (C) ')):
                availability.replaceChild(notice, paragraph)
                break
        else:
            prepend_element(availability, notice)
        declaration = f'<?xml version="{skeleton.version or "1.0"}"'
        if skeleton.standalone is not None:
            declaration += ' standalone="yes"' if skeleton.standalone else ' standalone="no"'
        return '\n'.join([declaration + ' ?>', *(node.toxml() for node in skeleton.childNodes), ''])


def write_output(header, base_dir, body_xml, shared_files):
    """Write the complete dictionary and its accompanying files."""
    print("Writing TEI dictionary…")
    before_body_end, body_end, trailer = header.rpartition('</body>')
    tei_file = os.path.join(base_dir, 'epo-eng.tei')
    if not os.path.exists(base_dir):
        os.mkdir(base_dir)
    with open(tei_file, 'w', encoding='utf-8') as f:
        f.write(before_body_end.rstrip() + '\n')
        f.write(body_xml)
        f.write(body_end + trailer)

    fd_import.output.reindent_xml(tei_file)
    if not os.path.exists(os.path.join(base_dir, 'COPYING')):
        # retrieve copyright information
        print("Downloading CC unported 3.0 license")
        with open(os.path.join(base_dir, 'COPYING'), 'wb') as f:
            req = urllib.request.Request(
                'https://creativecommons.org/licenses/by/3.0/legalcode.txt',
                data=None,
                headers = {
                    'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64; rv:54.0) Gecko/20100101 Firefox/54.0'
                })
            with urllib.request.urlopen(req) as u:
                f.write(u.read())

    # write Makefile
    fd_import.output.mk_makefile(base_dir, ['epo-eng.tei', 'epo-eng.patch'])
    for source in shared_files:
        shutil.copy(source, base_dir)


def main(input_file, tei_skeleton, output_directory, shared_dir):
    shared_files = [Path(shared_dir) / name for name in SHARED_FILES]
    for source in shared_files:
        if not source.is_file():
            raise FileNotFoundError(f'Missing shared file: {source}')
    print("Parsing dictionary…")
    with open(input_file, 'r', encoding='utf-8-sig') as f:
        release, author = parse_attribution(f.readline())
        words = [(n, line.rstrip('\n')) for n, line in enumerate(f, start=2)
                 if line.strip() and not line.startswith(' ')]

    word_list = []
    for lnum, word_pair in words:
        if word_pair.lstrip().startswith("#"):
            continue
        try:
            head, trans = word_pair.split(' : ')
            translations = tokenize(trans, literal_brackets=True, keep_medial_parentheses=True)
            translations = structure_translations(translations)
            annotation = re.fullmatch(r'(.+?)\s+\(([^()]+)\)', head.strip())
            if annotation:
                head = HeadWord(*annotation.groups())
            else:
                head = HeadWord(head.strip())
            word_list.append((head, translations))
        except ValueError as v:
            raise ValueError(f"line {lnum}: {''.join(v.args)}")

    # enrich words with grammatical info, strip "to" and other words *and*
    # convert to XML
    xml = []
    for head, trans in word_list:
        # translations might get altered, e.g. "to" is stripped
        gram, trans = guess_grammar_details(trans)
        xml += ['<entry>', head.as_xml()]
        if gram:
            xml.append(f'<gramGrp>\n{gram}\n</gramGrp>')
        xml += translations_to_xml(trans) + ['</entry>']

    skeleton_path = Path(tei_skeleton).resolve()
    if skeleton_path == (Path(output_directory) / 'epo-eng.tei').resolve():
        raise ValueError("The skeleton and output dictionary must have distinct paths")
    header = updated_header(skeleton_path, release, author, len(word_list))
    write_output(header, output_directory, '\n'.join(xml), shared_files)
    if skeleton_path.read_text(encoding='utf-8') != header:
        with tempfile.TemporaryDirectory(dir=skeleton_path.parent) as tmp:
            temporary = Path(tmp) / skeleton_path.name
            temporary.write_text(header, encoding='utf-8')
            shutil.copymode(skeleton_path, temporary)
            temporary.replace(skeleton_path)

def check_args():
    """command line parameter validation"""
    if len(sys.argv) != 5:
        print("Error: invalid command line parameters.")
        import textwrap
        print(f"Usage: {sys.argv[0]} <INPUT_FILE> <TEI SKELETON> <OUTPUT_DIRECTORY> <SHARED_DIR>\n    ", end="")
        print('\n    '.join(textwrap.wrap(("TEI SKELETON has to be a FreeDict TEI file, with an empty "
                "body tag. The opening and closing body tags have to be on "
                "separate lines.\n"
                "The output directory may exist and files will be overwritten, "
                "as necessary."), 78)))
        sys.exit(1)

    if not os.environ['FREEDICT_TOOLS']:
        print("The environment variable FREEDICT_TOOLS is not set.")
        print("This is mandatory, so that the necessary scripts can be located.")
        print("Have a look at the FreeDict wiki for more information:")
        print("https://github.com/freedict/fd-dictionaries/wiki")
        sys.exit(4)
    for file in sys.argv[1:3]:
        if not os.path.exists(file):
            print(f'Sorry, but the file "{file}" does not exist.')
            sys.exit(2)
    dictdir = os.path.basename(os.path.abspath(sys.argv[3]))
    if len(dictdir) != 7 or '-' not in dictdir:
        print("The output directory has to be named with the usual FreeDict naming scheme. Otherwise, the Make rules will fail.")
        sys.exit(5)

if __name__ == '__main__':
    check_args()
    try:
        main(*sys.argv[1:])
    except FileNotFoundError as error:
        sys.exit(str(error))

