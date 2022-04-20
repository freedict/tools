#!/usr/bin/env -S sed -Ef
#
# de-en/drop.sed - drop some information that cannot be properly parsed yet
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

s`\{(adj), usually not used before a noun\}`{\1}`
s`\{(prp)\; \+ Fall des jeweiligen Bezugsworts\}`{\1}`
s`\{(\+Gen\.)/(bei|von etw\.)\}`{\1}`g
s`\{(prp\; \+Dat\.) / von jdm\./etw\.\}`{\1}`
s` *\{Quantifikator\}``g

# TEI does not specify an element for degree of comparison.
s` *\{\+ *(Superlativ|superlative)\}``g

# This should probably become <colloc>[prp +]</colloc>
s` *\{prp *\+\}``g


# vi: noet
