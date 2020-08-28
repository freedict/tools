#!/usr/bin/env -S sed -nEf
#
# extract_switchexps.sed - a small script to extract "<>"-expressions.
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

# Note: Only the succeeding word is taken with, to prevent the list of results
#       from becoming too long.
#       (TODO:) Do the inverse, separately.

s/.*(<> *[^ ;|:]+)/\1\n/g
s/(<> *[^ ;|:]+).*/\1/p
