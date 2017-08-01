#!/usr/bin/env python3
# (C) 2017 Sebastian Humenda
# License: GPL3+
"""
Ding2tei works in two phases:

1.  Split each dictionary definition (one per line) into headwords and
    translations, then tokenize the most important bits:

    -   {…} - grammatical info
    -   […] category
    -   (…) anything, really
    -   "|", "," and ";" cause extra events

Afterwards, the tokens are restructured into an intermediate format,
which resembles roughly the TEI format, but allows for an more abstract
handling. This abstract representation, once constructed, can do
wild-guessing on element content to figure out details of the plain text
notation. I.e. whether (…) contain a collocate word or smash to
translation into one. Some of this wild-guessing is language-specific
and might need additional work for a specific dictionary. For instance,
"waste (disposal) consultant" → "waste consultants" and "waste disposal
consultants".
"""

import enum
from os.path import abspath, dirname
import sys

import intermediate

sys.path.append(dirname(dirname(abspath(sys.argv[0]))))
import tokenizer


#pylint: disable=too-few-public-methods
class Event(enum.Enum):
    Comma = 0
    Semicolon = 1
    VerticalBar = 2
    Grammar = 3
    Usage = 4


class Element:
    def __init__(self, content, type):
        self.content = content
        self.type = type

    def __repr__(self):
        return "<Elem(%s): '%s'>" % (self.type.name, self.content)


class Tokenizer:

    def __init__(self, filename):
        self.filename = filename
        self.entries = []

    def parse(self):
        with open(self.filename, encoding='utf-8') as f:
            for line in f:
                if not line.lstrip().startswith('#') and line.strip():
                    # obtain left-hand side and right-hand side of translation
                    entry = []
                    for side in (e.rstrip().lstrip() for e in line.split(' :: ')):
                        side = self.tokenize(side)
                        side = self.postprocess(side)
                        entry.append(side)
                    self.entries.append(entry)


    def tokenize(self, piece):
        events = []
        current_token = ''
        #pylint: disable=used-before-assignment
        def add_token_and_reset_it(current_token):
            """Add `current_token` to `events`, clear `current_token`."""
            current_token = current_token.lstrip().rstrip()
            if current_token:
                events.append(current_token)
                return ''
            else:
                return current_token

        within_braces = False
        for ch in piece:
            if within_braces:
                if ch == '}':
                    within_braces = False
                current_token += ch
            else: # not within {}, recognize characters
                if ch == ',':
                    current_token = add_token_and_reset_it(current_token)
                    events.append(Event.Comma)
                elif ch == ';':
                    current_token = add_token_and_reset_it(current_token)
                    events.append(Event.Semicolon)
                elif ch == '|':
                    current_token = add_token_and_reset_it(current_token)
                    events.append(Event.VerticalBar)
                elif ch == '{':
                    within_braces= True
                    current_token += ch
                else:
                    current_token += ch
        add_token_and_reset_it(current_token) # add last parsed chunk
        return events

    def postprocess(self, oldtokens):
        # safe opening and closing syntax element and the function which
        # processes this syntax element; dictionary would be possible, but
        # grammar stuff must be parsed first
        syntax_elements = [(('{', '}'), self.extract_grammatical_info),
                (('[', ']'), self.extract_usage_and_notes)]
        for (opening, closing), transformator in syntax_elements:
            tokens = []
            for token in oldtokens:
                if not isinstance(token, str):
                    tokens.append(token)
                else:
                    if opening in token and closing in token:
                        tokens.extend(transformator(token))
                    else:
                        tokens.append(token)
            oldtokens = tokens[:]
            tokens = []
        return oldtokens

    def extract_grammatical_info(self, token):
        tokens = []
        before, within, after = self.extract_area(token, '{', '}')
        if before:
            tokens.append(before)
        tokens.append(Element(within, Event.Grammar))
        if after:
            tokens.append(after)
        return tokens


    def extract_usage_and_notes(self, token):
        tokens = []
        before, within, after = self.extract_area(token, '[', ']')
        if before:
            tokens.append(before)
        tokens.append(Element(within, Event.Usage))
        if after:
            if '[' in after and ']' in after:
                tokens.extend(self.extract_area(after.lstrip(), '[', ']'))
            else:
                tokens.append(after)
        return tokens

    def extract_area(self, string, start, end):
        start = string.find(start)
        end = string.find(end)
        text_before = (string[:start] if start > 0 else '').rstrip()
        text_after = (string[end+1:] if end < (len(string)-1) else '').lstrip()
        return (text_before, string[start+1:end], text_after)



class ParserError(Exception):
    pass

class TokenRestructurer:
    def parse(self, entry):
        # iterate over headword: is_headword (bool), translation: is_headword
        info = ((tokenizer.split_list(entry[0], Event.VerticalBar), True),
                (tokenizer.split_list(entry[1], Event.VerticalBar), False))
        entry = intermediate.Entry()
        for alternatives, is_head in info:
            for alternative in self.handle_forms(alternatives, is_head=is_head):
                entry.add_child(alternative)
        return entry
                    

    def handle_forms(self, events, is_head):
        form_nodes = []
        mk_node = lambda: (intermediate.Form if is_head else intermediate.Sense)
        for synset in tokenizer.split_list(events, Event.Semicolon):
            outer_forms = []
            for synonym in tokenizer.split_list(synset, Event.Comma):
                form = self.handle_word(mk_node(), synonym)
                outer_forms.append(form)
            if len(outer_forms) == 0:
                raise ParserError("Comma encountered, but no words parsed, possibly a bug. Text: %s" % \
                        repr(synset))
            # if outer has one child and inner has only one synonym, we can
            # throw away the additional level
            elif len(outer_forms) == 1 and len(outer_forms[0]) == 1:
                form_nodes.append(outer_forms[0])
            else:
                outer_form = mk_node()()
                for f in outer_forms:
                    outer_form.add_child(f)
                form_nodes.append(outer_form)
        return form_nodes

    def handle_word(self, cls, tokens):
        form = cls(repr(tokens))
        return form



def main():
    tk = Tokenizer('de-en.txt')
    tk.parse()
    t = TokenRestructurer()
    for headwords_and_translations in tk.entries[:10000]:
        entry = t.parse(headwords_and_translations)
        print(entry)

if __name__ == '__main__':
    main()
