#! /usr/bin/env bash
# 
# Copyright (c) <year> Antoine Gagné
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

declare -r AUTHORS="Antoine Gagné"
declare -r PROGRAM_NAME="$(basename "${0}")"
declare -r PROGRAM_DIRECTORY="$(readlink -m "$(dirname "${0}")")"
declare -ra ARGUMENTS=("${@}")

usage() {
    cat <<- EOF
		Usage: ${PROGRAM_NAME} [-h|--help] [-V|--version]
		Do something.
		Example: ${PROGRAM_NAME} -h

		Available options:
		  -h, --help     display this help text and exit
		  -V, --version  display version information and exit
	EOF
}

version() {
    cat <<- EOF
		${PROGRAM_NAME} v0.0.0

		Written by ${AUTHORS}
	EOF
}

parse_command_line_arguments() {
    local -r _arguments="$(parse_long_options "${@}")"
    parse_short_options "${_arguments}"
}

parse_long_options() {
    local -r _arguments=("${@}")
    local _parsed_arguments

    local _argument
    for _argument in "${_arguments[@]}"; do
        case "${_argument}" in
            --help)
                _parsed_arguments="${_parsed_arguments}-h "
                ;;
            --version)
                _parsed_arguments="${_parsed_arguments}-V "
                ;;
            *)
                _parsed_arguments="${_parsed_arguments}${_argument} "
                ;;
        esac
    done

    echo "${_parsed_arguments}"
}

parse_short_options() {
    while getopts "hV" OPTION; do
        case ${OPTION} in
            V)
                version
                exit 0
                ;;
            h)
                usage
                exit 0
                ;;
            *)
                usage
                exit 1
                ;;
        esac
    done
}


main() {
    parse_command_line_arguments "${@}"
}

main "${@}"
