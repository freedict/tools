#!/usr/bin/env -S sed -Ef
#
# preprocess/alter.sed - syntax alterations
#
# Copyright 2020 Einhard Leichtfuß
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

# TODO: This does not catch "ppron pl" in grouped annotations (\{.*[,;/].*\}).
#       (where it does not occur, in the version 1.8.1 of the Ding).
#       This would possibly better be handled in the parser.
s`\{(ppron) (pl)\}`\{\1; \2\}`g

# {relativ} is not an indpendent annotation.
# Assimilate, with {pron interrog} in mind.
# Note: It can be assumed that both below parts never occcur in (separate)
#       groups when belonging to one another (since then their relation would
#       not be obvious).
s`\{(pron)\} *\{(relativ)\}`\{\1 \2\}`g


## [.or.] -> [./.]
# Notes:
#  * Cannot be generalized (as of now (2020-09-03), since [.] may contain free
#    text.
#  * Delegating to parser is generally possible, however requires splitting by
#    whitespace in the lexer.
#    * Which causes some (solvable) problems.

s`\[(obs\.) or (geh\.)\]`[\1/\2]`g
s`\[(geh\.) or (humor\.)\]`[\1/\2]`g
s`\[(formal) or (obs\.|humor\.)\]`[\1/\2]`g


# vi: noet
