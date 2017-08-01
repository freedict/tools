"""This file imports the epo-eng dictionary from
<http://www.denisowski.org/Esperanto/ESPDIC/espdic_readme.htm>. Please see the
README for more information."""

import enum
import os
import shlex
import shutil
import sys
import urllib.request
import xml.sax.saxutils as saxutils

# allow tokenizer import
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(sys.argv[0]))))
from tokenizer import ChunkType, tokenize


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
        return '<cit>\n<def>%s</def>\n</cit>' % saxutils.escape(self.definition)

class Word:
    #pylint: disable=too-few-public-methods
    """This is either a head word or a translation."""
    def __init__(self, word, usage_info=None):
        self.word = word
        # gramGrp is a list of ready-to-use xml strings, basically *one* node
        self.gramGrp, self.usage_info = self.__get_gram_info(usage_info)
        self.word_type = type

    def __repr__(self):
        return ('%s (%s)' % (self.word, self.usage_info) if self.usage_info else
                self.word)

    def __get_gram_info(self, usage_info):
        """Figure out, whether text is a collocating word or a usage hint.
        Return tuple ready-to-use XML. """
        # special cases: catch collocations
        collocations = ['for', 'of', 'up', 'to', 'in', 'on', 'by', 'upon',
            'ke', 'de', "do"] # esperanto
        if not usage_info:
            return (None, None)
        elif usage_info in collocations:
            return (['<colloc>%s</colloc>' % saxutils.escape(usage_info)], None)
        else:
            return (None, '<usg type="hint">%s</usg>' %
                    saxutils.escape(usage_info))

    def as_xml(self):
        xml = ['<cit type="trans">\n<quote>', saxutils.escape(self.word),
                '</quote>']
        if self.usage_info:
            xml.append('\n' + self.usage_info) # already escaped
        if self.gramGrp:
            xml.append('\n<gramGrp>\n%s\n</gramGrp>' % '\n'.join(self.gramGrp))
        return ''.join(xml + ['\n</cit>'])

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
        if not self.type == WordType.Full:
            extent = ' extent="%s"' % self.type.value
        orth = '<orth%s>%s</orth>' % (extent, saxutils.escape(self.word))
        if self.gramGrp:
            orth += '\n<gramGrp>\n%s</gramGrp>' % '\n'.join(self.gramGrp)
        if self.usage_info:
            orth += '\n' + self.usage_info
        return '<form>\n%s\n</form>' % orth

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
                    raise ValueError(("Couldn't parse translations; tokens "
                        "parsed: %s\nCurrent token: %s\nTokens left: %s") % \
                                (translations,chunk,unordered_list))
        elif chunk[0] == ChunkType.Word:
            # word with usage info / colloc
            if unordered_list and unordered_list[0][0] == ChunkType.Paren:
                # in some cases definitions are written as "to (be in) a
                # position", this is poor markup. ignore those parenthesis and
                # add it as a whole word:
                if len(unordered_list) >= 2 and unordered_list[1][0] == ChunkType.Word:
                    translations[-1].append(Word(chunk[1] +" " + \
                            unordered_list.pop(0)[1] + " " + \
                            unordered_list.pop(0)[1]))
                else:
                    translations[-1].append(Word(chunk[1],
                        unordered_list.pop(0)[1]))
            else:
                translations[-1].append(Word(chunk[1]))
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

def guess_grammar_details(translations):
    """This function inspects the English translations to infer grammatical
    information. Definitions starting with "to " are verbs in the infinitive
    form, etc. The translations might get altered, for instance, to strip the
    "to " from the actual definition."""
    gram = ''
    firstword = translations[0][0]
    if isinstance(firstword, Definition):
        return (None, translations) # don't fiddle around with def's
    if firstword.word.startswith('to '):
        gram = '<pos>v</pos>'
        for deflist in translations:
            for word in deflist:
                word.word = word.word.lstrip('to ')
    # remove wrongly parsed colloc's
    collocs = ['<colloc>%s</colloc>' % c for c in ['to', 'a', 'an']]
    for deflist in translations:
        for word in deflist:
            if word.gramGrp: # it need to have a colloc, otherwise no checks
                for colloc in collocs:
                    if colloc in word.gramGrp:
                        idx = collocs.index(colloc)
                        del word.gramGrp[idx]
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

def write_output(base_dir, tei_skeleton, body_xml):
    """This writes the dictionary import into the specified directory."""
    print("Writing TEI dictionary…")
    with open(tei_skeleton, 'r', encoding='utf-8') as f:
        header = f.read()
    body_start = header.find('<body>') + 6
    tei_file = os.path.join(base_dir, 'epo-eng.tei')
    os.mkdir(base_dir)
    with open(tei_file, 'w', encoding='utf-8') as f:
        f.write(header[:body_start] + '\n')
        f.write(body_xml)
        f.write(header[body_start+1:].lstrip().rstrip() + '\n')
    if shutil.which('xmllint'):
        print('Reindenting file…')
        unfmt = os.path.join(base_dir, 'epo-eng-unfmt.tei')
        os.rename(tei_file, unfmt)
        ret = os.system('xmllint --format %s > %s' % (shlex.quote(unfmt),
            shlex.quote(tei_file)))
        if not ret:
            os.remove(unfmt)
        else:
            print("Exiting due to previous error.")
            sys.exit(ret)
    else:
        print("Xmllint is not installed. It is strongly advised to do so, "
            "otherwise no validation and reindentation happens.")

    # retrieve copyright information
    print("Downloading CC unported 3.0 license")
    with open(os.path.join(base_dir, 'COPYING'), 'wb') as f:
        req = req = urllib.request.Request(
                'https://creativecommons.org/licenses/by-sa/3.0/legalcode',
                data=None,
                headers = {
                    'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64; rv:54.0) Gecko/20100101 Firefox/54.0'
                })
        with urllib.request.urlopen(req) as u:
            f.write(u.read())

    # copy README
    shutil.copy('README.dict', os.path.join(base_dir, 'README'))
    # write Makefile
    with open(os.path.join(base_dir, 'Makefile'), 'w', encoding='utf-8') as f:
        f.write("""# The line below is really just a fallback and only works if you have got a copy of the tools directory at this location. It's better to set the environment variable in your shell.
FREEDICT_TOOLS ?= ../../tools
DISTFILES = COPYING epo-eng.patch epo-eng.tei freedict-P5.xml freedict-P5.rng \
        freedict-P5.dtd freedict-dictionary.css INSTALL Makefile NEWS README
# do not generate phonemes
supported_phonetics =

include $(FREEDICT_TOOLS)/mk/dicts.mk
""")


def main(input_file, tei_skeleton, output_directory):
    print("Parsing dictionary…")
    with open(input_file, 'r', encoding='utf-8') as f:
        # gnerator with word pairs; ignore indented lines (only file header,
        # ATM)
        words = (wp for wp in f.read().split('\n')
            if wp.strip() and not wp.startswith(" "))

    word_list = []
    for word_pair in words:
        head, trans = word_pair.split(' : ')
        translations = tokenize(trans)
        translations = structure_translations(translations)
        head = tokenize(head)
        if len(head) == 2: # headword with definition
            head = HeadWord(head[0][1], head[1][1])
        else:
            head = HeadWord(head[0][1])
        word_list.append((head, translations))

    # enrich words with grammatical info, strip "to" and other words *and*
    # convert to XML
    xml = []
    for head, trans in word_list:
        # translations might get altered, e.g. "to" is stripped
        gram, trans = guess_grammar_details(trans)
        xml += ['<entry>', head.as_xml()]
        if gram:
            xml.append('<gramGrp>\n%s\n</gramGrp>' % gram)
        xml += translations_to_xml(trans) + ['</entry>']

    write_output(output_directory, tei_skeleton, '\n'.join(xml))
    print("Done. Now it's time to copy DTD, CSS and RNG and validate the dictionary.")

def check_args():
    """command line parameter validation"""
    if len(sys.argv) != 4:
        print("Error: invalid command line parameters.")
        import textwrap
        print("Usage: %s <INPUT_FILE> <TEI SKELETON> <OUTPUT_DIRECTORY>\n    "\
                % sys.argv[0], end="")
        print('\n    '.join(textwrap.wrap(("TEI SKELETON has to be a FreeDict TEI file, with an empty "
                "body tag. The opening and closing body tags have to be on "
                "separate lines.\n"
                "The output must not exist."), 78)))
        sys.exit(1)

    if not os.environ['FREEDICT_TOOLS']:
        print("The environment variable FREEDICT_TOOLS is not set.")
        print("This is mandatory, so that the necessary scripts can be located.")
        print("Have a look at the FreeDict wiki for more information:")
        print("https://github.com/freedict/fd-dictionaries/wiki")
        sys.exit(4)
    for file in sys.argv[1:3]:
        if not os.path.exists(file):
            print('Sorry, but the file "%s" does not exist.' % file)
            sys.exit(2)
    if os.path.exists(sys.argv[3]):
        print("The output directory may not exist.")
        sys.exit(3)
    dictdir = os.path.basename(os.path.abspath(sys.argv[3]))
    if not len(dictdir) == 7 or '-' not in dictdir:
        print("The output directory has to be named with the usual FreeDict naming scheme. Otherwise, the Make rules will fail.")
        sys.exit(5)

if __name__ == '__main__':
    check_args()
    main(sys.argv[1], sys.argv[2], sys.argv[3])

