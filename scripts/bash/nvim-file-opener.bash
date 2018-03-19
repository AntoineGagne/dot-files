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

declare LOG_FILE

usage() {
    cat <<- EOF
		Usage: ${PROGRAM_NAME} [-h|--help] [-V|--version]
		Do something.
		Example: ${PROGRAM_NAME} -h

		Available options:
		  -l FILE, --logfile=FILE  where to log the output
		  -h, --help               display this help text and exit
		  -V, --version            display version information and exit
	EOF
}

version() {
    cat <<- EOF
		${PROGRAM_NAME} v0.0.0

		Written by ${AUTHORS}
	EOF
}

get_extension() {
    local -r _extension="${1##*.}"

    echo "${_extension}"
}

strip_extension() {
    local -r _stripped_filename="${1%.*}"

    echo "${_stripped_filename}"
}

is_program_installed() {
    local -r _program_name="${1}"
    type "${_program_name}" >/dev/null
}

trim() {
    local _string="${1}"

    echo "${_string}" | xargs
}

die() {
    local -r _message="${1}"

    echo "${_message}" >&2
    exit 1
}

is_root() {
    [[ ${EUID} -eq 0 ]]
}

parse_command_line_arguments() {
    local -r _arguments="$(parse_long_options "${@}")"
    local -r _parsed_arguments_number="$(parse_short_options "${_arguments}")"

    echo "${_parsed_arguments_number}"
}

split_long_options() {
    local -r _long_option="${1}"

    echo "${_long_option}" | awk -F '=' '{
        for (i = 2; i <= NF; ++i) {
            printf "%s ", $i
        };
    }'
}

parse_long_options() {
    local -r _arguments=("${@}")
    local _parsed_arguments

    local _argument
    for _argument in "${_arguments[@]}"; do
        case "${_argument}" in
            --logfile=?*)
                _parsed_arguments="${_parsed_arguments}-l${_argument#*=}"
                ;;
            --logfile=)
                die '--logfile requires a non-empty option argument.'
                ;;
            --help)
                _parsed_arguments="${_parsed_arguments}-h "
                ;;
            --version)
                _parsed_arguments="${_parsed_arguments}-V "
                ;;
            --)
                break
                ;;
            *)
                _parsed_arguments="${_parsed_arguments}${_argument} "
                ;;
        esac
    done

    echo "${_parsed_arguments}"
}

parse_short_options() {
    while getopts ":hVl:" OPTION; do
        case ${OPTION} in
            V)
                version
                exit 0
                ;;
            h)
                usage
                exit 0
                ;;
            l)
                LOG_FILE="$(trim "${OPTARG}")"
                ;;
            :)
                die "Option -${OPTARG} requires an argument." >&2
                ;;
            \?)
                die "Invalid option: -${OPTARG}" >&2
                ;;
        esac
    done

    echo "${OPTIND}"
}


main() {
    local -r _parsed_arguments_number="$(parse_command_line_arguments "${@}")"
    shift "${_parsed_arguments_number}"
    exec &> >(tee -a "${LOG_FILE:-${TEMPDIR:-/tmp}/${PROGRAM_NAME}.log}")

    spawn_terminal "${@}"
}

spawn_terminal() {
    local -ra _files="${*}"
    exec urxvtc -e bash -i -c "nvim ${_files}"
}

main "${@}"
