#! /bin/sh
#
# Copyright © 2019 Antoine Gagné
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

readonly AUTHORS='Antoine Gagné'
readonly PROGRAM_NAME="$(basename "${0%.*}")"
readonly REQUIRED_COMMANDS='pdftk'
readonly REQUIRED_VARIABLES=''

exec 3>/dev/null

usage() {
    cat <<- EOF
Usage: ${PROGRAM_NAME} [-h|--help] [-V|--version] [-v|--verbose] <pdf> <expression>
Remove watermark from a PDF file.
Example: ${PROGRAM_NAME} -h

Available options:
  -h, --help                            display this help text and exit
  -v, --verbose                         enable verbose output
  -V, --version                         display version information and exit
EOF
}

version() {
    cat <<- EOF
${PROGRAM_NAME} v1.0.0

Written by ${AUTHORS}
Licensed under AGPL3
EOF
}

die() {
    _message="${1}"
    echo "${_message}" 1>&2
    exit 1
}

warn() {
    _message="${1}"
    echo "${_message}" 1>&2
}

verbose() {
    exec 3>&1
}

info() {
    _message="${1}"
    echo "${_message}" 1>&3
}

is_variable_unset() {
    _name="${1}"

    eval "[ \"\${${_name}:-x}\" = \"x\" ]"
}

validate_mandatory_variables() {
    for _name in ${REQUIRED_VARIABLES}; do
        if is_variable_unset "${_name}"; then
            die "${_name} is unset. Exiting."
        fi
    done
}

is_program_installed() {
    _program_name="${1}"
    command -v "${_program_name}" >/dev/null
}

validate_dependencies() {
    for _command in ${REQUIRED_COMMANDS}; do
        if ! is_program_installed "${_command}"; then
            die "${_command} is not installed. Exiting."
        fi
    done
}

is_program_installed() {
    _program_name="${1}"
    command -v "${_program_name}" >/dev/null
}

uncompress() {
    _input="${1}"
    _output="${2}"

    pdftk "${_input}" output "${_output}" uncompress
}

remove_watermark() {
    _expression="${1}"
    _input="${2}"
    _output="${3}"

    sed -e "s@${_expression}@@g" "${_input}" > "${_output}"
}

compress() {
    _input="${1}"
    _output="${2}"

    pdftk "${_input}" output "${_output}" compress
}

setup_cleanup() {
    _files="${*}"


    trap "{ rm --force ${_files}; }" EXIT
}

main() {
    validate_dependencies
    validate_mandatory_variables

    set -x
    _pdf="${1}"
    _expression="${2}"

    if ! _temporary_file="$(mktemp)"; then
        die 'Failed to create temporary file'
    fi

    _uncompressed_file="${_temporary_file}-uncompressed.pdf"
    _unwatermarked_file="${_temporary_file}-unwatermarked.pdf"
    _fixed_file="${_temporary_file}-fixed.pdf"
    setup_cleanup "${_temporary_file}" "${_uncompressed_file}" "${_unwatermarked_file}" "${_fixed_file}"

    # Most of the commands below were adapted from:
    # https://superuser.com/a/536644
    if ! uncompress "${_pdf}" "${_uncompressed_file}"; then
        die "Failed to uncompress ${_pdf} to ${_uncompressed_file}"
    fi

    if ! remove_watermark "${_expression}" "${_uncompressed_file}" "${_unwatermarked_file}"; then
        die "Failed to remove watermark from ${_uncompressed_file} to ${_unwatermarked_file} using expression '${_expression}'"
    fi

    if ! compress "${_unwatermarked_file}" "${_fixed_file}"; then
        die "Failed to compress ${_unwatermarked_file} to ${_fixed_file}"
    fi

    if ! mv "${_fixed_file}" "${_pdf}"; then
        die "Failed to move ${_fixed_file} to ${_pdf}"
    fi
}

while getopts ':hvV-:' OPTION; do
    case "${OPTION}" in
        V)
            version
            exit 0
            ;;
        h)
            usage
            exit 0
            ;;
        v)
            verbose
            ;;
        -)
            case "${OPTARG}" in
                help)
                    usage
                    exit 0
                    ;;
                version)
                    version
                    exit 0
                    ;;
                verbose)
                    verbose
                    ;;
                *)
                    die "Invalid option --${OPTARG}."
                    ;;
            esac
            ;;
        :)
            die "Option -${OPTARG} requires an argument."
            ;;
        \?)
            die "Invalid option: -${OPTARG}."
            ;;
    esac
done

shift "$((OPTIND - 1))"
main "${@}"
