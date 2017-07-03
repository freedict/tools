"""This file imports the epo-eng dictionary from
<http://www.denisowski.org/Esperanto/ESPDIC/espdic_readme.htm>. Please see the
README for more information."""

import enum
import os
import sys

class ChunkType(enum.Enum):
    Word = 0
    Paren = 1 # parenthesized expressions
    Semicolon = 2 # homonyms
    Comma = 3 # word/definition boundary

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
        return '<cit>\n<def>%s</def>\n</cit>' % self.definition

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
            return (['<colloc>%s</colloc>' % usage_info], None)
        else:
            return (None, '<usg type="hint">%s</usg>' % usage_info)

    def as_xml(self):
        xml = ['<cit type="trans">\n<quote>', self.word, '</quote>']
        if self.usage_info:
            xml.append('\n' + self.usage_info)
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
        orth = '<orth%s>%s</orth>' % (extent, self.word, )
        if self.gramGrp:
            orth += '\n<gramGrp>\n%s</gramGrp>' % '\n'.join(self.gramGrp)
        if self.usage_info:
            orth += '\n' + ''.join(self.usage_info)
        return '<form>\n%s\n</form>' % orth

#pylint: disable=redefined-variable-type
def parse_tokens(source):
    """Tokenize translations into chunks, where each chunk is a tuple of (ChunkType,
    content)."""
    chunks = []
    tmp_storage = '' # for unfinished chunks
    # state can be either default "word" or within parenthesis (paren)
    state = ChunkType.Word
    prevchar = ''
    for ch in source:
        if state == ChunkType.Paren:
            if ch == ')':
                state = ChunkType.Word
                chunks.append((ChunkType.Paren, tmp_storage.strip()))
                tmp_storage = ''
            else:
                tmp_storage += ch
        else: # ChunkType.Word
            if ch == '(' and (prevchar == '' or prevchar.isspace()):
                state = ChunkType.Paren
                if tmp_storage.strip():
                    chunks.append((ChunkType.Word, tmp_storage.strip()))
                    tmp_storage = ''
            elif ch == ',': # comma outside of parens, new word
                if tmp_storage.strip(): # not empty
                    chunks.append((ChunkType.Word, tmp_storage.strip()))
                tmp_storage= ''
                chunks.append((ChunkType.Comma, None))
            elif ch == ';':
                if tmp_storage.strip(): # not empty
                    chunks.append((ChunkType.Word, tmp_storage.strip()))
                chunks.append((ChunkType.Semicolon, None))
                tmp_storage= ''
            else:
                tmp_storage += ch
        prevchar = ch
    if tmp_storage:
        chunks.append((ChunkType.Word, tmp_storage.strip()))
    return chunks

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
        else:
            raise BaseException("Unhandled case.")
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

def main(input_file):
    with open(input_file, 'r', encoding='utf-8') as f:
        # gnerator with word pairs; ignore indented lines (only file header,
        # ATM)
        words = (wp for wp in f.read().split('\n')
            if wp.strip() and not wp.startswith(" "))

    word_list = []
    for word_pair in words:
        head, trans = word_pair.split(' : ')
        translations = parse_tokens(trans)
        translations = structure_translations(translations)
        head = parse_tokens(head)
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
    print('\n'.join(xml))


# command line parameter validation
if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Error: invalid command line parameters.")
        print("Usage: %s <INPUT_FILE>" % sys.argv[0])
        sys.exit(1)
    elif not os.path.exists(sys.argv[1]):
        print('Sorry, but the file "%s" does not exist.' % sys.argv[1])
        sys.exit(2)
    else:
        main(sys.argv[1])

