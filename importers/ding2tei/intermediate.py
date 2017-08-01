"""This file contains all intermediate data structures to represent dictionary
entries. It can only support a subset of TEI and is intended to reflect the
features of the Ding format."""

import abc

class SemNode(abc.ABC):
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
                            " of strings as 'text'." % self.__class__.__name__)
            else:
                if isinstance(text, str):
                    self.__text = [text] # always keep list
                else:
                    raise TypeError("%s excepts only strings as 'text'"
                             % self.__class__.__name__)
        else:
            self.__text = None
        self.__children = []
        self.__attrs = {}

    def get_attributes(self):
        return self.__attrs

    def add_text(self, text):
        if not isinstance(text, str):
            raise TypeError("Text can be only of type str.")
        if self.__text is None:
            self.__text = [text]
        else:
            self.__text.append(text)

    def add_attr(self, key, val):
        self.__attrs[key] = val

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

    def __len__(self):
        """Return the number of text entries."""
        return len(self.__text)

    def __getitem__(self, idx):
        return self.__children[idx]

    def __repr__(self):
        text = ('' if not self.__text else " text: '%s';" % self.__text)
        children = ('' if not self.__children else ' (%s)' % ', '.join(
                repr(c) for c in self.__children))
        name = self.__class__.__name__.split('.')[-1]
        return '<%s %s%s>' % (name, text, children)

class GramGrp(SemNode):
    def __init__(self):
        super().__init__()
        self.__pos = None
        self.__gender = None

    def set_gender(self, gen):
        self.__gender = gen

    def set_pos(self, pos):
        self.__pos = pos

class Definition(SemNode):
    pass

class Translation(SemNode):
    accept_multiple_text_entries = True

class Usage(SemNode):
    accept_multiple_text_entries = True

class Sense(SemNode):
    allowed_children = (Translation, Definition, Usage, GramGrp) # try to avoid top-level gram

class Form(SemNode):
    """A form is one or more words, optionally containing grammar elements as
    well. Forms can be nested and they can have attributes.
    Note: allowed_children cannot contain Form in the class definition, because
    the name is not defined during class creation. Therefore, the Form class is
    added to allowed_children at the end of this module."""
    allowed_children = (GramGrp)
    accept_multiple_text_entries = False
    allowed_attributes = ['infl']


class Entry(SemNode):
    allowed_children = (Sense, Form) # try to avoid top-level gram

# Please see Form.__doc__ for an explanation of this line
Form.allowed_children = (Form, GramGrp)
