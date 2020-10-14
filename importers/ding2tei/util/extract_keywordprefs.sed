#!/usr/bin/env -S sed -nEf
#
# extract_keywordprefs.sed - extract words prefixing certain keywords
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

# Find all prefixing words (use ł as separator)
:loop
s`^(.*ł)? *([^ ł]+) +((etw|sth|sb|jm?d[nms]?|so)\.|oneself)`\1\2ł`
t loop
s`ł *$``
t print
s`^(.*ł)? *([^ ł]+)`\1`
t loop

# Print all found words from hold space.
:print
s` +$``
/^$/ d
s`ł`\n`g
p

# vi: noet
