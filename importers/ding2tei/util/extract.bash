#!/usr/bin/env bash
#
# extract.bash - a small script to extract different expressions
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

dingfile=../dict/ding/de-en.txt.pp
types=(
	{brace,bracket,paren,dot,switch}exps nonalpha words wordpairs
	parenprefs{1,2}
	keyword{prefs,sufs} keywordswithprefs
	)

dir="$(dirname "$(realpath "$0")")"
outdir="${dir}/results"


# Do not overwrite existing files (using '>').
set -o noclobber


function main()
{
	cd "$dir"

	mkdir -p "$outdir"

	if [ $# -ge 1 ]
	then
		extract "$1"
	else
		for type in "${types[@]}"
		do
			printf 'Extracting %s...\n' "$type"
			extract "$type"
		done
	fi
}


function extract()
{
	local type="$1"
	local shfile="extract_${type}.sh"
	local sedfile="extract_${type}.sed"
	local script

	if [ -e "$shfile" ]
	then
		script="$shfile"
	else
		if ! [ -e "$sedfile" ]
		then
			printf 'Error: %s does not exist.\n' "$sedfile" 1>&2
			exit 1
		fi

		script="$sedfile"
	fi

	exps_sorted="$(./"$script" < "$dingfile" | sort)"

	# Sort by name.
	uniq <<< "$exps_sorted" > "${outdir}/${type}.by_name"

	# Sort by number of occurences.
	uniq -c <<< "$exps_sorted" | sort -nr > "${outdir}/${type}.by_count"

	# Sort by length.
	awk '{ print length, $0 }' < "${outdir}/${type}.by_name" | sort -n \
		| cut -d ' ' -f 2- > "${outdir}/${type}.by_length"
}


main "$@"


# vi: ts=2 sw=2 noet
