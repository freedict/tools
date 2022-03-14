#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/alter.sed - syntax alterations
#
# Copyright 2020,2022 Einhard Leichtfuß
#
# This file is part of ding2tei-haskell.
#
# ding2tei-haskell is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ding2tei-haskell is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with ding2tei-haskell.  If not, see <https://www.gnu.org/licenses/>.
#

# {relativ} is not an indpendent annotation.
# Assimilate, with {pron interrog} in mind.
# Note: It can be assumed that both below parts never occcur in (separate)
#       groups when belonging to one another (since then their relation would
#       not be obvious).
s`\{(pron)\} *\{(relativ)\}`\{\1 \2\}`g

# New in version 1.9 (rare); change to old syntax (see also above).
s`\{(relativ)\.(pron)\}`{\2 \1}`g

# Adapt grammar information in {+ X} to match the usual syntax for X.
s`\{\+ *(Zahl|number)}`{+num}`g
s`\{\+ *verb}`{+v}`g
s`\{\+ *(Einzahl|singular)\}`{+sing}`g

# Not actually alteration of syntax, but of content to fit syntax.
s`\<(R/S ratio) >(1)\>`\1 > \2`g


# vi: noet
