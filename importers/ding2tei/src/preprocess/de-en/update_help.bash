#!/usr/bin/env bash
#
# preprocessed/de-en/update_help.bash - help updating to new Ding version
#
# Copyright 2022 Einhard Leichtfuß
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


# When updating ding2tei for a new Ding version, the sed scripts should also
# be revised.
#  * Some replacements may become obsolete.
#  * Some replacements may need changing.
#  * Note: It may be worthwile to keep some generic replacements even if they
#    do not apply to anything anymore.
#  * The course for adapting to a new Ding version is to simply run the main
#    ding2tei program on the preprocessed Ding dictionary and act upon any
#    errors.
#    * Also, ideally, non-critical syntax errors/inconsistencies (those that
#      do not break the parser), should be adressed (see util/).
#
# This script can also be helpful whenever amending the sed files to check
# that they actually applied.
#
# Precondition:
#  * The sed scripts contain only single line commands.
#
# How to use this script:
#  * See help() / run `$0 help`.
#  1. Run `init'.
#  2. `apply' all old scripts to the old source.
#     * This creates, for each command line of each script a new file, which
#       are ordered in sequence.
#  3. Copy all scripts to a folder `new/'.
#  4. for each sedfile (in order specified by $order_conf_file_relative)
#     1. `apply' this script to the new source (use script index).
#        * Again, this yields for each sed command in the current script a
#          new file.
#     2. Open the current sed script in `new/` in an editor (still identical
#        to the old sed script).
#     3. Run your favourite `diff' program (I suggest `vimdiff') for each
#        subsequent pair of newly created files, both for the old and new
#        Ding source file.
#        * A small bash for loop is handy (TODO: include in this script).
#          - "a=(SEDFILE_INDEX_PADDED.*)
#             for (( i=0; i < ${#a[@]} - 1; i++ ))
#             do
#               vimdiff "${a[i]}" "${a[i+1]}"
#             done
#            "
#        1. Compare the `diff's manually, with the current sed command in
#           mind.
#        2. Adapt the current sed script in `new/`.
#        * Note: The scripts are identical as of now, so there is a direct
#          correspondence between the diffed old and new files.
#     4. `apply' this script again (if anything changed).
#     5. Verify that all changes applied correctly.
#        * Ideally manually.  A cmp(1) may be deemed sufficient, though.
#        * A `diff' of the old and new script files is helpful.
#     6. (optional) Check the diffs again (if anything changed).
#        * Note: The scripts are distinct now, so there is not necessarily
#          a direct correspondence between the old and new files anymore.
#     * This can be repeated.  One can also change the order (in particular,
#       change things in an "earlier" sed file later.
#       * One should be aware, however, that the changes may build upon
#         another; therefore, one would ideally check whether all the later
#         changes still apply as expected.
#  5. (optional) `apply` the whole new script list again (in one run), check
#     everything.
#  6. Replace the old scripts by the new scripts.
#
# See also:
#  * src/preprocess/de-en/README
#  * src/preprocess/de-en/all.bash

typeset -r script_path="$(realpath "$0")"
typeset -r script_dir="${script_path%/*}"
typeset -r base_dir="$script_dir"

order_conf_file_relative='order.conf.bash'

typeset -r auxdir_relative='aux'
typeset -r auxdir_absolute="${base_dir}/${auxdir_relative}"
typeset -rA sedscriptdir_relative=(
	[old]='.'
	[new]='new'
)
typeset -rA sedscriptdir_absolute=(
	[old]="${base_dir}/${sedscriptdir_relative[old]}"
	[new]="${base_dir}/${sedscriptdir_relative[new]}"
)

# Get ${sedfiles_ordered[@]}.
source "${base_dir}/${order_conf_file_relative}"


function help()
{
	cat << EOF
Syntax:
  \$0 init OLD_DING_SRCFILE NEW_DING_SRCFILE
  \$0 apply old|new I_START I_END

\`init' must be run before the first \`apply'.

When running \`apply', I_START must not be larger than the last I_END.
The first \`apply' must be run with 0 as I_START.
\`apply' drops any possibly existing output (of previous runs of \`apply')
with I larger than I_START before starting.

The old sed scriptfiles must be located in \`${sedscriptdir_relative[old]}/',
the new sed scriptfiles in \`${sedscriptdir_relative[new]}/'.

The new sedfiles should initially just be a copy of the old ones and can then
be adapted.  Only sedfiles specified in \`${order_conf_file_relative}'
are used, and all these are expected to exist.

Results of this script will be found in \`${auxdir_relative}/'.

All relative paths above relative to:
\`${base_dir}'
EOF
}


# Implementation notes:
#  * sed file indices and sed file line numbers, when part of a string (e.g.,
#    in filenames) are always padded to be 3 and 5 characters long.
#    - This is expected to be sufficient.
#    - The 3 and 5 are hard-coded in many places.
#    - The intention behind padding is to have the numerical and alphabetical
#      sort order of filenames coincide.
#    - An error is issued if the number of files or lines is too big.


set -o noclobber
set -o errexit
set -o nounset
shopt -s extglob


function main()
{
	[[ $# -ge 1 ]] || { help; exit 1; }
	local -r cmd="$1"
	shift 1

	case "$cmd" in
		init)
			init "$@"
			;;
		apply)
			apply "$@"
			;;
		help)
			help
			;;
		*)
			help
			exit 1
	esac
}


function init()
{
	[[ $# -eq 2 ]] || { help; exit 1; }
	local -rA ding_srcfile=(
		[old]="$1"
		[new]="$2"
	)

	for oldnew in old new
	do
		mkdir -p -- "${auxdir_absolute}/sed/${oldnew}"
		mkdir -p -- "${auxdir_absolute}/data/${oldnew}"
		cp "${ding_srcfile[$oldnew]}" "${auxdir_absolute}/${oldnew}.ding.txt"
		ln -sf -- "../../${oldnew}.ding.txt" \
			"${auxdir_absolute}/data/${oldnew}/000.00000.txt"
	done
}


function apply()
{
	[[ $# -eq 3 ]] || { help; exit 1; }
	local -r oldnew="$1"
	local -r i_start="$2"
	local -r i_end="$3"
	verify_string_old_or_new "$oldnew"
	verify_is_sedfile_index "$i_start"
	verify_is_sedfile_index "$i_end"
	verify_less_or_equal "$i_start" "$i_end"

	# Delete all data that is to be rewritten or "later" s.t. the state remains
	# consistent.
	clean "$oldnew" "$i_start" inifinity

	# i_* sed file index
	local -i i_cur i_next
	local si_cur si_next
	for (( i_cur = i_start; i_cur <= i_end; i_cur++ ))
	do
		local sedfile="${sedfiles_ordered[i_cur]}"
		printf -v si_cur '%03i' "$i_cur"
		printf -v si_next '%03i' "$(( i_cur + 1 ))"
		sed -E -- '/^\s*$/ d; /^#/ d' \
			"${sedscriptdir_absolute[$oldnew]}/${sedfile}" \
			> "${auxdir_absolute}/sed/${oldnew}/${si_cur}.${sedfile}"

		# j_*: sed file line index
		local lines
		mapfile -t lines < "${auxdir_absolute}/sed/${oldnew}/${si_cur}.${sedfile}"
		local -i j_cur=0 j_next
		local sj_cur sj_next
		for line in "${lines[@]}"
		do
			j_next=j_cur+1
			printf -v sj_cur '%05i' "$j_cur"
			printf -v sj_next '%05i' "$j_next"
			sed -E -- "$line" \
				< "${auxdir_absolute}/data/${oldnew}/${si_cur}.${sj_cur}.txt" \
				> "${auxdir_absolute}/data/${oldnew}/${si_cur}.${sj_next}.txt" 
			j_cur=j_next
		done

		ln -s -- "${si_cur}.${sj_next}.txt" \
			"${auxdir_absolute}/data/${oldnew}/${si_next}.00000.txt"
	done
}


function clean()
{
	[[ $# -eq 3 ]] || { help; exit 1; }
	local -r oldnew="$1"
	local -r i_start="$2"
	local i_end="$3"
	verify_string_old_or_new "$oldnew"
	verify_type_int "$i_start"
	if [[ "$i_end" == inifinity ]]
	then
		# No, this is not exactly efficient (TODO).
		i_end=999
	else
		verify_type_int "$i_end"
	fi
	local -r i_end
	verify_less_or_equal "$i_start" "$i_end"

	local -i i_cur
	for (( i_cur = i_start; i_cur <= i_end; i_cur++ ))
	do
		local si_cur si_next
		printf -v si_cur '%03i' "$i_cur"
		printf -v si_next '%03i' "$((i_cur + 1))"

		if [[ -v sedfiles_ordered[i_cur] ]]
		then
			local sedfile="${sedfiles_ordered[i_cur]}"
			rm -f "${auxdir_absolute}/sed/${oldnew}/${si_cur}.${sedfile}"
		fi

		rm -f "${auxdir_absolute}/data/${oldnew}/${si_cur}".!(00000).txt
		rm -f "${auxdir_absolute}/data/${oldnew}/${si_next}.00000.txt"
	done
}


function verify_type_int()
{
	if [[ "$1" =~ ^[0-9]+$ ]]
	then
		local -i i="$1"
		if [[ "$i" == "$1" ]]
		then
			return 0
		fi
	fi

	printf 'Error: Not an int: %s\n' "$1" >&2
	exit 1
}


function verify_is_sedfile_index()
{
	verify_type_int "$1"
	if ! [[ "$1" -ge 0 && "$1" -lt "${#sedfiles_ordered[@]}" ]]
	then
		printf 'Error: sedfile index out of range: %s not in [%s,%s)\n' \
			"$1" '0' "${#sedfiles_ordered[@]}" >&2
		exit 1
	fi
}


function verify_less_or_equal()
{
	if ! [[ "$1" -le "$2" ]]
	then
		printf 'Error: %s greater than %s.\n' "$1" "$2" >&2
		exit 1
	fi
}


function verify_sedfile_count()
{
	# 999 is already bad because there would be created a final symlink with
	# 999+1 = 1000.
	if [[ "$1" -ge 999 ]]
	then
		printf 'Error: Number of sed files too large: %s\n' "$1" >&2
		exit 1
	fi
}

function verify_line_count()
{
	local -r file="$1"
	local -r lc="$2"
	if [[ "$lc" -ge 100000 ]]
	then
		printf 'Error: Number of lines too large: %s: %s\n' "$file" "$lc" >&2
		exit 1
	fi
}


function verify_string_old_or_new()
{
	if [[ "$1" != @(old|new) ]]
	then
		printf 'Error: Invalid argument: %s\n' "$1" >&2
		printf "Must be \`old' or \`new'\\n"
	fi
}


main "$@"
