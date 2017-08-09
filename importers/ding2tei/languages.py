"""This module contains the language-specific pre- and postprocessors."""

# forgive me, ubt I actually really need all of them
from dictstructure import AbstractParser, ChunkType, Form, GramGrp, \
        ParserError, Translation, Unprocessed, Usage

class EngDeuParser(AbstractParser):
    GENDER = ['n', 'm', 'f'] # ontology?
    NUMBER = {'pl': 'pl', 'sing': 'sg', 'no pl': 'no pl'}
    POS = {'adj':'adj', 'adv': 'adv', 'art': 'art', 'conj': 'conj',
            'idiom': 'phrase', 'interj': 'int',
            'n': 'n', 'num': 'num',
            'prp': 'prep', 'prep': 'prep', 'ppron': 'pron', 'pron': 'pron',
            'v': 'v', 'vi': 'vi', 'vr': 'vr', 'vt': 'vt', 'vti': 'vti'}
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
                return g
            elif len(tokens) == 2: # past participle, etc. of a verb
                f = node_class(text=tokens)
                f.add_attr("type", "infl")
                return f
            else:
                # ToDo: raise
                print(ParserError("Couldn't recognize expression: {%s}" % text))
                return GramGrp() # ToDo, remove, dummy

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




