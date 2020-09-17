#!/usr/bin/env -S sed -Ef
#
# extract_parenprefs.sed - a small script to extract prefixes inside parens
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

# Notes:
#  * This script should receive lines with parenthese-expressions.
#  * Prefixes consisting of a two words extracted.

# Drop lines containing single words inside parens
/^\( *[^ ]* *\)/ d

s`^\( *([^ )]+ [^ )]+).*$`\1`

# vi: noet
