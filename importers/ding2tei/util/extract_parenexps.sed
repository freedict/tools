#!/usr/bin/env -S sed -nEf
#
# extract_parenexps.sed - a small script to extract paren-expressions
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

/\(.*\)/ {

	# Reduce to (roughly) "\([^()]*\)*"
	s/[^(]*[^)]*(\([^)]*\))([^(]*[^)]*$)?/\1/g

	# Separate by newlines.
	s/\)\(/\)\n\(/g

	# Print
	p
}

# vi: noet
