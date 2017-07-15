#!/usr/bin/env python3
# (C) 2017 Sebastian Humenda
# License: GPL3+

import enum


def genid(headword):
    return headword # ToDo

class Entry:
    def __init__(self, headwords, translations):
        self.headwords = headwords
        self.translations = translations

class Element:
    class Event(enum.Enum):
        Comma = 0
        Semicolon = 1
        VerticalBar = 2
        Grammar = 3
        Usage = 4

    def __init__(self, content, type):
        self.content = content
        self.type = type

    def __repr__(self):
        return "<Elem(%s): '%s'>" % (self.type.name, self.content)

class StructuralParser:
    def split_list(self, old, delimiter):
        new = [[]]
        for item in old:
            if item == delimiter:
                new.append([])
            else:
                new[-1].append(item)
        return new


    def parse_wordlist(self, wordlist):
        # source language:
        # ToDo:
        return wordlist


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
                    events.append(Element.Event.Comma)
                elif ch == ';':
                    current_token = add_token_and_reset_it(current_token)
                    events.append(Element.Event.Semicolon)
                elif ch == '|':
                    current_token = add_token_and_reset_it(current_token)
                    events.append(Element.Event.VerticalBar)
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
        tokens.append(Element(within, Element.Event.Grammar))
        if after:
            tokens.append(after)
        return tokens


    def extract_usage_and_notes(self, token):
        tokens = []
        before, within, after = self.extract_area(token, '[', ']')
        if before:
            tokens.append(before)
        tokens.append(Element(within, Element.Event.Usage))
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



def main():
    tokenizer = Tokenizer('de-en.txt')
    tokenizer.parse()
    sparser = StructuralParser()
    entries = []
    for lhs, rhs in tokenizer.entries[:900]:
        headwords = sparser.parse_wordlist(lhs)
        translations = sparser.parse_wordlist(rhs)
        entries.append(Entry(headwords, translations))
    for entry in entries[:900]:
        print(entry.headwords, entry.translations)

if __name__ == '__main__':
    main()
