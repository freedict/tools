"""Tokenizer for text-only dictionary formats.

To use this library, add your dictionary importer as a subdirectory to
importers/. Assuming you have an immporter in importers/foo, you would add the
following snippet to your foo.py to use this module:

    from os import abspath, dirname
    import sys

    sys.path.append(dirname(dirname(abspath(sys.argv[0]))))

    import tokenizer

For the usage, please see the functions and classes defined in this module."""

import enum


class ChunkType(enum.Enum):
    """The tokenizer parses individual chunks and emits tuples with type
    information and chunk content. The first entry is the chunk type, as defined
    by this enum."""
    Word = 0
    Paren = 1 # parenthesized expressions ()
    Bracket = 2 # brackets []
    Brace = 3 # "embraced" expressions: {}
    Semicolon = 4 # homonyms
    Comma = 5 # word/definition boundary

def tokenize(source):
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
the outer wins. This wont work:

    ([(blah)])

will result in

    [(Paren, '[(blah'), (Word, '])')]
"""
    chunks = []
    tmp_storage = [] # for unfinished chunks
    def save_parsed_chunk(ctype):
        """Save tmp_storage in chunks with the given type. This function is a short-hand."""
        nonlocal chunks, tmp_storage
        chunks.append((ctype, ''.join(tmp_storage).strip()))
        tmp_storage = []
    state = ChunkType.Word # current state of parser (ChunkType)
    prevchar = ''
    for ch in source:
        if state in (ChunkType.Paren, ChunkType.Bracket, ChunkType.Brace):
            if ch == ')' and state == state.Paren:
                state = ChunkType.Word
                save_parsed_chunk(ChunkType.Paren)
            elif ch == ']' and state == ChunkType.Bracket:
                state = ChunkType.Word
                save_parsed_chunk(ChunkType.Bracket)
            elif ch == '}' and state == ChunkType.Brace:
                state = ChunkType.Word
                save_parsed_chunk(ChunkType.Brace)
            else: # within the brace/bracket/parethesis
                tmp_storage.append(ch)

        else: # ChunkType.Word
            # only recognize parenthesized expressions, ifa separate word;
            # prevents "expression(s)" to be recognized as two words
            if ch in ('(', '[', '{') and (prevchar == '' or prevchar.isspace()):
                if ch == '(':
                    state = ChunkType.Paren
                elif ch == '[':
                    state = ChunkType.Bracket
                elif ch == '{':
                    state = ChunkType.Brace
                if tmp_storage: # add word before, if any
                    save_parsed_chunk(ChunkType.Word)
            elif ch == ',': # comma outside of parens, new word
                if tmp_storage: # not empty
                    save_parsed_chunk(ChunkType.Word)
                chunks.append((ChunkType.Comma, None))
            elif ch == ';':
                if tmp_storage: # not empty
                    save_parsed_chunk(ChunkType.Word)
                chunks.append((ChunkType.Semicolon, None))
            else:
                tmp_storage.append(ch)
        prevchar = ch
    if tmp_storage:
        save_parsed_chunk(ChunkType.Word)
    return chunks


