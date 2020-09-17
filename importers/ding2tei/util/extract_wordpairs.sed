#!/usr/bin/env -S sed -nEf
#
# extract_word.sed - a small script to extract words
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
#  * This script is very slow.
#  * The generated data is not overly useful, since <::> <|> et al. are
#    considered words.

/^#/ d

h
s/ *([^ ]+) +([^ ]+)/\1 \2\n/g
s/(^|\n) *[^ ]*$//
/^$/ d
p

x
s/^ *[^ ]+//
s/ *([^ ]+) +([^ ]+)/\1 \2\n/g
s/(^|\n) *[^ ]*$//
/^$/ d
p
