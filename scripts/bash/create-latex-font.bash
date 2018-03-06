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
declare FONT_FILE

declare -r TEXMF="$(kpsewhich --var-value=TEXMFLOCAL | awk -F ':' '{print $1}')"
declare -A FILE_HANDLER_BY_FONT_TYPE=(["ttf"]=handle_ttf_file ["otf"]=handle_otf_file)
declare -A DIRECTORY_NAME_BY_FONT_TYPE=(["ttf"]="truetype" ["otf"]="opentype")
declare -ra REQUIRED_PROGRAMS=("ttf2afm" "ttf2afm" "vptovf" "otfinfo" "curl" "otftotfm")

usage() {
    cat <<- EOF
		Usage: ${PROGRAM_NAME} [-h|--help] [-V|--version]
		Install fonts so that they are usable by pdftex.
		Example: ${PROGRAM_NAME} -h

		Available options:
		  -l FILE, --logfile=FILE          where to log the output
		  -i FONT_FILE, --input=FONT_FILE  the fonts to package
		  -h, --help                       display this help text and exit
		  -V, --version                    display version information and exit
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

split_long_options() {
    local -r _long_option="${1}"

    echo "${_long_option}" | awk -F '=' '{
        for (i = 2; i <= NF; ++i) {
            printf "%s ", $i
        };
    }'
}

die() {
    local -r _message="${1}"

    echo "${_message}" >&2
    exit 1
}

is_root() {
    [[ ${EUID} -eq 0 ]]
}

trim() {
    local _string="${1}"

    echo "${_string}" | xargs
}

parse_long_options() {
    local -r _arguments=("${@}")
    local _parsed_arguments

    local _argument
    for _argument in "${_arguments[@]}"; do
        case "${_argument}" in
            --logfile*)
                _parsed_arguments="${_parsed_arguments}-l$(split_long_options "${_argument}")"
                ;;
            --input*)
                _parsed_arguments="${_parsed_arguments}-i$(split_long_options "${_argument}")"
                ;;
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
    while getopts ":hVl:i:" OPTION; do
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
            i)
                FONT_FILE="$(trim "${OPTARG}")"
                ;;
            :)
                die "Option -${OPTARG} requires an argument." >&2
                ;;
            \?)
                die "Invalid option: -${OPTARG}" >&2
                ;;
        esac
    done
}

is_program_installed() {
    local -r _program_name="${1}"
    type "${_program_name}" >/dev/null
}

format_name() {
    local -lr _directory_name="$(echo "${1}" | awk -F ' ' '{
        for (i = 1; i <= NF; ++i) {
            printf "%s", $i
        };
    }')"

    echo "${_directory_name}"
}

get_extension() {
    local -r _extension="${1##*.}"

    echo "${_extension}"
}

strip_extension() {
    local -r _stripped_filename="${1%.*}"

    echo "${_stripped_filename}"
}

generate_font_file_hierarchy() {
    local -rl _font_family="$(format_name "${1}")"
    local -rl _font_type="${2}"
    local -r _file_hierarchy="texmf/fonts/${_font_type}/public/${_font_family}"

    mkdir -p "${_file_hierarchy}"
    echo "${_file_hierarchy}"
}

download_license() {
    local -r _license_url="${1}"
    local -r _file_hierarchy="${2}"

    curl --create-dirs --fail --location -o "${_file_hierarchy}/LICENSE" "${_license_url}"
}

handle_otf_file() {
    local -r _font_file="${1}"
    local -r _file_name="$(strip_extension "$(basename "${_font_file}")")"
    local -r _font_family="$(otfinfo -a "${_font_file}")"
    local -r _font_file_information="$(otfinfo -i "${_font_file}")"
    local -r _license_url="$(trim "$(echo "${_font_file_information}" | grep 'License URL:' | sed 's/License URL://')")"
    local -r _font_subfamily="$(trim "$(echo "${_font_file_information}" | grep 'Subfamily:' | sed 's/Subfamily://')")"

    local -r _file_hierarchy="$(generate_font_file_hierarchy "${_font_family}" "${DIRECTORY_NAME_BY_FONT_TYPE["otf"]}")"
    download_license "${_license_url}" "${_file_hierarchy}"
    otftotfm -a \
             -e texnansx "${_font_file}" \
             -fkern -fliga "LY1--${_file_name}"
}

handle_ttf_file() {
    local -r _font_file="${1}"
}

handle_font_file() {
    local -r _font_file="${1}"
    local -r _extension="$(get_extension "${_font_file}")"

    ${FILE_HANDLER_BY_FONT_TYPE[${_extension}]} "${_font_file}"
}

main() {
    parse_command_line_arguments "${@}"
    exec &> >(tee -a "${LOG_FILE:-${TEMPDIR:-/tmp}/${PROGRAM_NAME}.log}")

    for _program in "${REQUIRED_PROGRAMS[@]}"; do
        if ! is_program_installed "${_program}"; then
          echo "${_program} is not available."
          exit 1
        fi
    done

    if [[ -z "${FONT_FILE}" ]]; then
        die "You must specify a font file."
    fi

    if [[ ! -f "${FONT_FILE}" ]]; then
        die "${FONT_FILE} is not a valid font file."
    fi

    handle_font_file "${FONT_FILE}"
}

main "${@}"
