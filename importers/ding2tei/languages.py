"""This module contains the language-specific pre- and postprocessors."""

import re

# forgive me, but I actually really need all of them
from dictstructure import AbstractParser, ChunkType, Definition, Form, \
        GramGrp, ParserError, Sense, Translation, Unprocessed, Usage

class DeuEngParser(AbstractParser):
    GENDER = ['n', 'm', 'f'] # ontology?
    NUMBER = {'pl': 'pl', 'sing': 'sg', 'no pl': 'no pl'}
    POS = {'adj':'adj', 'adv': 'adv', 'art': 'art', 'conj': 'conj',
            'idiom': 'phrase', 'interj': 'int',
            'n': 'n', 'num': 'num', 'prp': 'prep', 'prep': 'prep',
            'prn': 'pron', 'ppron': 'pron', 'pron': 'pron',
            'v': 'v', 'vi': 'vi', 'vr': 'vr', 'vt': 'vt', 'vti': 'vti'}
    CASE_RGX = re.compile(r"""([A-Z|a-z]\??)? # optional collocate with optional question mark
        \+\s*(Gen|Dat|Akk)\.? # grammatical case
            /?\s*(.*)$ # optional preposition/collocate
        """, re.VERBOSE)

    def recognize_gender_or_number(self, text):
        if text in self.GENDER:
            return GramGrp(pos='n', gender=text)
        elif text in self.NUMBER:
            return GramGrp(pos='n', number=self.NUMBER[text])
        # comma is either for multiple genders, for a gender and a "pl"
        # declaration or rarely for different verb forms
        elif ',' in text:
            g = GramGrp(pos='n') # it's a noun
            for token in (t.strip() for t in text.split(',')):
                if token in self.NUMBER:
                    g.number = self.NUMBER[token]
                elif token in self.GENDER:
                    g.add_child(GramGrp(gender=token))
                elif token in self.POS:
                    g.pos = None
                    g.add_child(GramGrp(pos=token))
                elif len(token) > 3 and ' ' in token: # usage hint
                    g.usg = token
                else:
                    raise ParserError("Unknown token in {%s}" % text)
            return g
        return None

    def handle_brace(self, node_class, chunk):
        text = chunk[1].rstrip().rstrip('.')
        # handle ; first, might contain , as well and that's checked by
        # recognize_gender_or_number
        if ';' in text:
            tokens = [t.strip() for t in text.split(';')]
            if all(t in self.POS for t in tokens):
                g = GramGrp()
                for t in tokens:
                    g.add_child(GramGrp(pos=t))
                outer.add_child(g)
            elif len(tokens) == 2: # past participle, etc. of a verb
                f = node_class(text=tokens)
                f.add_attr("type", "infl")
                return f
            else:
                # try to find case hints, etc.
                g = GramGrp()
                for token in tokens:
                    if token in self.POS:
                        g.pos = token
                        continue

                    match = self.CASE_RGX.search(token)
                    if match:
                        match = match.groups()
                        usg = Usage(match[1])
                        usg.add_attr('type', 'gram')
                        outer.add_child(usg)
                        for colloc in (x for x in (match[0], match[2]) if x):
                            if colloc in self.POS:
                                g.pos = self.POS[colloc] # missing semicolon to POS
                            else:
                                g.colloc = colloc
                    else:
                        # ToDo: raise
                        print(ParserError("Couldn't recognize expression: {%s}" % text))
                        return # ToDo, remove, dummy
                outer.add_child(g)

        gram = self.recognize_gender_or_number(text)
        if gram:
            return gram
        elif '/' in text: # only seen that for vt/vi
            tokens = text.split('/' if '/' in text else ';')
            if not all(t.strip() in self.POS for t in tokens):
                # ToDo: when stabilized, make it raise
                print(ParserError("Encountered unknown (POS) token {%s}" % repr(chunk)))
                return chunk
            g = GramGrp() # no info, only children have
            for pos in (p for p in tokens if p in self.POS):
                g.add_child(GramGrp(pos=self.POS[pos.strip()]))
            return g
        elif text.strip() in self.POS:
            return GramGrp(pos=self.POS[text])
        else:
            # ToDo: make it raise as soon as all questions about it resolved
            print(NotImplementedError(chunk))
            return chunk

    def handle_paren(self, chunks):
        isword = lambda i: len(chunks[i]) == 2 and chunks[i][0] == ChunkType.Word
        isparen = lambda i: len(chunks[i]) == 2 and chunks[i][0] == ChunkType.Paren
        idx = 0 # index into chunks
        enough_chunks_left = lambda num: idx + num < len(chunks)
        while idx < len(chunks):
            # "foo (bar) baz (explanation)"
            if enough_chunks_left(4) and isword(idx) and isparen(idx+1) and \
                    isword(idx+2) and isparen(idx+3):
                idx += 4
            else:
                idx += 1

    def handle_unprocessed(self, outer):
        while outer.get_children() and isinstance(outer.get_children()[0],
                Unprocessed):
            unprocessed = outer.get_children().pop(0).get_text()
            required_inner = (Form if outer.__class__ == Form else Translation)
            start = 0
            while start <= (len(unprocessed)-1):
                chunk = unprocessed[start]
                if chunk[0] == ChunkType.Brace:
                    node = self.handle_brace(required_inner, chunk)
                    # ToDo: that if should be useless, but not all braces are parsed yet
                    if not isinstance(node, (tuple)):
                        outer.add_child(node)
                    else:
                        print("ignoring",chunk)
                elif chunk[0] == ChunkType.Bracket:
                    outer.add_child(Usage((chunk[1],)))
                # these are really, really hard to parse, therefore no parsing is tone
                # at all
                elif chunk[0] == ChunkType.Paren or chunk[0]  == ChunkType.Word:
                    start = self.attach_merged_text_and_paren(outer, unprocessed, start)
                elif chunk[0] == ChunkType.Slash: # abbreviation
                    f = required_inner([chunk[1]])
                    f.add_attr("type", "abbr")
                    outer.add_child(f)
                else:
                    raise ParserError("Unhandled chunk: " + repr(chunk))
                start += 1
        return outer

    def attach_merged_text_and_paren(self, outer, chunks, start):
        end = start
        reconstructed = []
        while end < len(chunks) and chunks[end][0] in (ChunkType.Word,
                ChunkType.Paren):
            reconstructed.append(chunks[end][1] if chunks[end][0] == ChunkType.Word
                    else '(%s)' % chunks[end][1])
            end += 1
        if isinstance(outer, (Form,)):
            outer.add_text(' '.join(reconstructed))
        else:
            outer.add_child(Translation([' '.join(reconstructed)]))
        return end - 1

    def simplify_markup(self, node):
        return node


class SpaDeuParser(DeuEngParser):
    def __init__(self):
        self.POS['Demonstrativpronomen'] = 'pron'

    # handle a few grammatical things differently
    def handle_brace(self, node_class, chunk):
        text = chunk[1].rstrip().rstrip('.')
        if text == 'mf' or text == 'fm':
            g = GramGrp(pos='n')
            for c in text:
                g.add_child(GramGrp(gender=c))
            return g
        elif text == 's': # 'that's gender n
            return super().handle_brace(node_class, (chunk[0], 'n'))
        else:
            return super().handle_brace(node_class, chunk)

    def simplify_markup(self, entry):
        getall = lambda x, y: [c for c in enumerate(x.get_children()) if
                isinstance(c[1], y)]
        for elem in [Form, Sense]:
            children = getall(entry, elem)
            if len(children) == 1:
                inner = getall(children[0][1], elem)
                if len(inner) > 0:
                    entry.pop(0)
                for child in inner:
                    entry.add_child(child[1])

        if any_pos(entry, 'adj'):
            forms = getall(entry, Form)
            if len(forms) == 2: # adjective, both male and female form
                # first only has word, second has GramGrp
                if forms[0][1].num_children() == 0 and forms[1][1].num_children() == 1:
                    second_form = entry.pop(forms[1][0])
                    for text in second_form.get_text():
                        forms[0][1].add_text(text)
                    # re-add gramgrp element, only child of second form
                    forms[0][1].add_child(second_form.get_children()[0])
        for elem in [Form, Sense]:
            children = [c[1] for c in getall(entry, elem)]
            for child in children:
                children.extend(c[1] for c in getall(child, elem)) # grandchildren
                gramgrps = getall(child, GramGrp)
                if len(gramgrps) > 1:
                    newgram = GramGrp(pos=gramgrps[0][1].pos)
                    deleted = 0
                    for idx, grp in gramgrps:
                        child.pop(idx - deleted)
                        deleted += 1
                        for attr in ['gender', 'number', 'usg']:
                            data = getattr(grp, attr)
                            if data: # gram. info present
                                setattr(newgram, attr, data)
                    child.add_child(newgram)
        return entry

def any_pos(node, pos):
    node_pos = False
    if isinstance(node, (GramGrp)):
        node_pos = node.pos == pos
    for child in node.get_children():
        node_pos = node_pos or any_pos(child, pos)
    return node_pos

