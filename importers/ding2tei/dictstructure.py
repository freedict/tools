"""This file contains all intermediate data structures to represent dictionary
entries. It can only support a subset of TEI and is intended to reflect the
features of the Ding format."""

import abc



from fd_import import tokenizer
from fd_import.tokenizer import ChunkType

class ParserError(Exception):
    pass


class SemNode:
    """Semantic node, docs."""
    allowed_children = () # no children allowed by default
    accept_multiple_text_entries = False
    allowed_attributes = []

    def __init__(self, text=None):
        super().__init__()
        if text:
            if self.accept_multiple_text_entries:
                if isinstance(text, (list, tuple)):
                    self.__text = text
                else:
                    raise TypeError("%s excepts only a tuple or a list" \
                            " of strings as 'text', got %s." % \
                            (self.__class__.__name__, repr(text)))
            else:
                if isinstance(text, str):
                    self.__text = [text] # always keep list
                else:
                    raise TypeError("%s excepts only strings as 'text'"
                             % self.__class__.__name__)
        else:
            self.__text = []
        self.__children = []
        self.__attrs = {}

    def get_text(self):
        """Return a list of text (tokens) of a node."""
        return self.__text

    def get_attributes(self):
        return self.__attrs

    def clear_text(self):
        self.__text.clear()

    def add_text(self, text):
        if not isinstance(text, (str, tuple, list)):
            raise TypeError("Text can be only of type str, list or tuple.")
        if isinstance(text, (tuple, list)):
            self.__text.extend(text)
        else: # string
            self.__text.append(text)

    def add_attr(self, key, val):
        self.__attrs[key] = val

    def get_all(self, childtype):
        """Return a generator object, returning all direct children with the
        requested type."""
        if not isinstance(childtype, (SemNode)):
            raise TypeError("Only objects of type SemNode can be searched for.")
        return (c for c in self.__children if isinstance(c, childtype))

    def get_all_with_idx(self, childtype):
        """Return a generator object, returning all direct children with the
        requested type."""
        return (c for c in enumerate(self.__children) if isinstance(c,
            childtype))

    def get_children(self):
        return self.__children

    def add_child(self, child):
        if not isinstance(child, self.allowed_children):
            allowed = ('only %s allowed' % ', '.join(repr(e) for e in self.allowed_children) \
                    if self.allowed_children else 'no children allowed')
            raise TypeError("%s cannot have %s as a child, %s" %
                    (self.__class__.__name__, type(child), allowed))
        self.__children.append(child)

    def num_children(self):
        return len(self.__children)

    def pop(self, idx):
        return self.__children.pop(idx)

    def __len__(self):
        """Return the number of text entries."""
        return len(self.__text)

    def __getitem__(self, idx):
        return self.__children[idx]

    def __repr__(self):
        text = ('' if not self.__text else " text: %s" % repr(self.__text))
        children = ('' if not self.__children else ' (%s)' % ', '.join(
                repr(c) for c in self.__children))
        name = self.__class__.__name__.split('.')[-1]
        return '<%s%s%s>' % (name, text, children)

    def __bool__(self):
        return True # False by default


class Unprocessed(SemNode):
    accept_multiple_text_entries = True
    def remove(self, index):
        """Remove object from the internal list of text objects."""
        del self.get_text()[index]

class GramGrp(SemNode):
    def __init__(self, pos=None, gender=None, number=None):
        super().__init__()
        self.pos = pos
        self.gender = gender
        self.number = number
        self.usg = None

    def __repr__(self):
        tks = list(filter(None,
            ((self.pos if self.pos else ''),
                (self.gender if self.gender else ''),
                (self.number if self.number else ''),
                (self.usg if self.usg else ''))))
        return '<%s (%s)>' % (self.__class__.__name__.split('.')[-1],
                ', '.join(tks))

class Definition(SemNode):
    pass

class Translation(SemNode):
    accept_multiple_text_entries = True

class Usage(SemNode):
    accept_multiple_text_entries = True

class Sense(SemNode):
    allowed_children = () # see end of module

class Form(SemNode):
    """A form is one or more words, optionally containing grammar elements as
    well. Forms can be nested and they can have attributes.
    Note: allowed_children cannot contain Form in the class definition, because
    the name is not defined during class creation. Therefore, the Form class is
    added to allowed_children at the end of this module."""
    allowed_children = (GramGrp)
    accept_multiple_text_entries = True
    allowed_attributes = ['type']


class Entry(SemNode):
    allowed_children = (Sense, Form) # try to avoid top-level gram


class AbstractParser:
    def parse(self, entry):
        is_bar = lambda x: x[0] == ChunkType.VerticalBar
        # iterate over headword: is_headword (bool), translation: is_headword
        info = ((tokenizer.split_list(entry[0], is_bar), True),
                (tokenizer.split_list(entry[1], is_bar), False))
        entry = Entry()
        for sense_or_head, is_head in info:
            for alternative in sense_or_head:
                node = (Form() if is_head else Sense())
                for a in self.handle_forms(alternative, is_head=is_head):
                    node.add_child(a)
                entry.add_child(node)
        entry = self.simplify_markup(entry)
        return entry


    def handle_forms(self, events, is_head):
        form_nodes = []
        # for or sense
        mk_node = lambda: (Form if is_head else Sense)

        for synset in tokenizer.split_list(events,
                lambda c: c[0] == ChunkType.Semicolon):
            outer_form = mk_node()()
            for synonym in tokenizer.split_list(synset,
                    lambda c: c[0] == ChunkType.Comma):
                outer_form.add_child(Unprocessed(synonym))
            try:
                form_nodes.append(self.handle_unprocessed(outer_form))
            except ParserError as p:
                p.args = list(p.args) + ['list of events ' + repr(events)]
                raise p
        return form_nodes

    @abc.abstractmethod
    def simplify_markup(self, node):
        return node

    @abc.abstractmethod
    def handle_unprocessed(self, outer):
        pass



# Please see Form.__doc__ for an explanation of this line
Form.allowed_children = (Form, GramGrp, Unprocessed, Usage)
Sense.allowed_children = (Sense, Translation, Unprocessed, Definition, Usage, GramGrp) # try to avoid top-level gram
GramGrp.allowed_children = (GramGrp)
Translation.allowed_children = (Usage, GramGrp)
