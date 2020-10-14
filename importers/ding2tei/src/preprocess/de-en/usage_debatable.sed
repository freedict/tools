#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/usage_debatable.sed - fix debatable errors in usage
#                                        annotations
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


### Normalize separator (<,>)
# Notes:
#  * In all but very few cases, the separator is a comma or a slash.
#  * Slash has a distinct meaning, the others (<;>, <und>) not.

s`\[(archaic)\; (academic)\]`[\1, \2]`g
s`\[(relig\.) und (Schw\.)\]`[\1, \2]`g


## <or> -> </>
# Notes:
#  * Cannot be generalized (as of now (2020-09-03), since [.] may contain free
#    text.
#  * Otherwise, it could be delegated to the parser.

s`\[(obs\.) or (geh\.)\]`[\1/\2]`g
s`\[(geh\.) or (humor\.)\]`[\1/\2]`g
s`\[(formal) or (obs\.|humor\.)\]`[\1/\2]`g


## Separation of extra info

# [] should conly contain well defined values, as opposed to ().
# Unfortunately, a little information gets lost this way.
# Too rare to consider in the parser.

s`\[(French) for (a female singer, especially in a nightclub)\]`[\1] (\2)`
s`\[(ugs\.) (schnelles Auto)\]`[\1] (\2)`g
s`\[(übtr\.) für (eine große, schlanke Person)\]`[\1] (\2)`g
s`\[(pej\.) für (Mittäter\; Gefolge)\]`[\1] (\2)`g
s`\[(obs\.) für (Küster, Kirchendiener)\]`[\1] (\2)`g


# vi: noet
