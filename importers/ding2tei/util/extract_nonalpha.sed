#!/usr/bin/env -S sed -Ef
#
# extract_nonalpha.sed - a small script to extract non-alphabetic characters
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

# Only innermost brace expressions ("{...}") are to be matched.
# (Actually, there are no nested brace expressions in the Ding source.)
# Several brace expressions in the same line are to be split into several
# lines.

# Remove alphabetic characters.
s/[[:alpha:]]//g

# Remove whitespace.
s/\s//g

# Remove empty lines.
/^$/ d

# Add trailing newlines.
s/./&\n/g
s/\n$//

# vi: noet
