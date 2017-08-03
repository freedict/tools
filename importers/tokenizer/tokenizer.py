"""Tokenizer for text-only dictionary formats.

To use this library, add your dictionary importer as a subdirectory to
importers/. Assuming you have an immporter in importers/foo, you would add the
following snippet to your foo.py to use this module:

    from os.path import abspath, dirname
    import sys

    sys.path.append(dirname(dirname(abspath(sys.argv[0]))))

    import tokenizer

For the usage, please see the functions and classes defined in this module."""

import enum
import inspect


class ChunkType(enum.Enum):
    """The tokenizer parses individual chunks and emits tuples with type
    information and chunk content. The first entry is the chunk type, as defined
    by this enum."""
    Word = 0
    Paren = 1 # parenthesized expressions ()
    Bracket = 2 # brackets []
    Brace = 3 # "embraced" expressions: {}
    Semicolon = 4 # delimiter, e.g. homonyms
    Comma = 5 # word/definition boundary
    VerticalBar = 6 # has to be surrounded by spaces, is a delimiter as e.g. comma
    Slash = 7 # optional; example usage include abbreviations "/ETC/"

def space_before_slash(string, i):
    """The tokenizer can recognize chunks enclosed by slashes. To avoid to many
    false positives, only expressions with no space are supported, ATM. This
    functions checks whether a space is found before the next slash."""
    for ch in string[i+1:]: # skip first character, is the very slash
        if ch.isspace():
            return True
        elif ch == '/':
            return False
    return True # end of string reached, is like a space


FRAMING = {'(': ChunkType.Paren, '[': ChunkType.Bracket, '{': ChunkType.Brace}
CLOSING = {')':'(', '}':'{', ']':'['}
def tokenize(source, parse_slash=False):
    #pylint: disable=redefined-variable-type,too-many-branches
    """Tokenize a line (of translations) into chunks, where each chunk is a tuple of (ChunkType,
content). This only works with either the headword or the translation part,
so instead of:

    abc - a, b; c
It either parses "abc" or "a,  b; c".

Chunk types with no value (as for instance Comma) return a tuple with None as
second element.

Parenthesis have precedence over commas and ;. So

    a, b (c,;d)

will be tokenized as

    [(Word, 'a'), (Comma, None), (Word, 'b'), (Paren, 'c,;d')]

The prefix ChunkType has been omitted for readibility reasons.
This precendence rule also means that nested structures are *not* recognized,
the outer wins. This won't work:

    ([(blah)])

will result in

    [(Paren, '[(blah'), (Word, '])')]
"""
    chunks = []
    tmp_storage = [] # for unfinished chunks
    def save_parsed_chunk(ctype):
        """Save tmp_storage in chunks with the given type. This function is a short-hand."""
        nonlocal chunks, tmp_storage
        if not all(c.isspace() for c in tmp_storage):
            chunks.append((ctype, ''.join(tmp_storage).strip()))
        tmp_storage = []
    state = ChunkType.Word # current state of parser (ChunkType)
    prevchar = ''

    is_new_word = lambda: prevchar == '' or prevchar.isspace()
    for idx, ch in enumerate(source):
        if state in FRAMING.values() or (parse_slash and state == ChunkType.Slash):
            if ch in CLOSING and state == FRAMING[CLOSING[ch]]:
                save_parsed_chunk(FRAMING[CLOSING[ch]])
                state = ChunkType.Word
            elif ch == '/' and state == ChunkType.Slash and parse_slash:
                save_parsed_chunk(ChunkType.Slash)
                state = ChunkType.Word
            else: # within the brace/bracket/parethesis
                tmp_storage.append(ch)

        else: # ChunkType.Word
            # separate a enclosed expression if preceeded by whitespace /
            # beginning of string; handle slash separately
            if is_new_word() and (ch in FRAMING or (parse_slash and \
                    ch == '/' and not space_before_slash(source, idx))):
                # above tests that / is followed by characters only, no spaces
                # and hence no straying slashes are detected
                if ch == '/' and parse_slash:
                    state = ChunkType.Slash
                else:
                    state = FRAMING[ch]
                if tmp_storage: # add word before, if any
                    save_parsed_chunk(ChunkType.Word)
            # only recognize /foo/ if parse_slash and if space in front
            elif ch == ',': # comma outside of parens, new word
                if tmp_storage: # not empty
                    save_parsed_chunk(ChunkType.Word)
                chunks.append((ChunkType.Comma, None))
            elif ch == ';':
                if tmp_storage: # not empty
                    save_parsed_chunk(ChunkType.Word)
                chunks.append((ChunkType.Semicolon, None))
            elif ch == '|' and (prevchar == '' or prevchar.isspace()): # space around?
                if tmp_storage: # not empty
                    save_parsed_chunk(ChunkType.Word)
                chunks.append((ChunkType.VerticalBar, None))

            else:
                tmp_storage.append(ch)
        prevchar = ch
    if tmp_storage:
        save_parsed_chunk(ChunkType.Word)
    return chunks



def split_list(mylist, delim):
    """Split a list into chunks with the given delimiter. If a delimiter is a
    function, it will get the list item as function and has to return true if a
    split should occur. Returned is an iterator, yielding a chunk at a time.
    Examples:

    >>> list(split_list([1,2,3,2,4,5,2,2,10], 2))
    [[1],[3],[4,5],[],10]]
    >>> list(split_list([]))
    []
    >>> list(split_list([1,2,3,4], 20))
    [[1,2,3,4]]"""
    if not mylist:
        return []
    gathered = []
    is_splitpoint = (delim if inspect.isfunction(delim)
                else lambda e: e == delim)
    for elem in mylist:
        if is_splitpoint(elem):
            yield gathered
            gathered = []
        else:
            gathered.append(elem)
    yield gathered

