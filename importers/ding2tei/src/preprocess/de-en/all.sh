#!/bin/sh
#
# preprocessed/de-en/all.sh - run all preprocessing scripts, in correct order
#
# Copyright 2020 Einhard Leichtfuß, 2021 the FreeDict project
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

# The Ding source is highly irregular.  The preprocessing steps help to keep
# the parser clean by transforming irregularities to a more regular markup.
#
# See also:
#  * src/preprocess/de-en/README


dir="$(dirname "$(realpath "$0")")"
cd "$dir"


./typos.sed \
	| ./grammar.sed \
	| ./inflected_forms.sed \
	| ./usage.sed \
	| ./usage_debatable.sed \
	| ./slashes.sed \
	| ./abbreviations.sed \
	| ./misc.sed \
	| ./quotes.sed \
	| ./additions.sed \
	| ./alter.sed \
	| ./drop.sed

# vi: ts=2 sw=2 noet
