#!/usr/bin/env -S sed -nEf
#
# test-ding.sed - validate the Ding input to a certain degree
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

# Note: This may give incorrect guesses when stumbling upon "/ | /".
#       - which is not present in Ding v1.8.1.

h

# Check for different count of <|> around <::>.
s/[^:|]//g
s/::/_/
s/://g
/_/ {
	/^(\|*)_\1/ ! {
		g
		s/^.*$/Unbalanced <\|> separators:\n&\n/
		p
	}
}

#g	-- add before next test

# vi: noet
