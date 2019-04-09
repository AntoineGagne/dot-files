#! /usr/bin/env bash
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

declare -r AUTHORS='Antoine Gagné'
declare -r PROGRAM_NAME="$(basename "${0%.*}")"
declare -r PROGRAM_DIRECTORY="$(readlink "$(dirname "${0}")")"
declare -r RED="$(tput setaf 9)"
declare -r RESET="$(tput sgr0)"
declare -ra REQUIRED_COMMANDS=()

repeat() {
    local -r _characters="${1}"
    local -ri _times="${2}"

    yes "${_characters}" | head -n "${_times}" | tr -d '\n'
}

usage() {
    local -r _padding="$(repeat ' ' "${#PROGRAM_NAME}")"
    cat <<- EOF
Usage: ${PROGRAM_NAME} [-h|--help] [-V|--version]
<Description>
Example: ${PROGRAM_NAME} -h

Available options:
  -h, --help                            display this help text and exit
  -V, --version                         display version information and exit
EOF
}

version() {
    cat <<- EOF
${PROGRAM_NAME} v0.1.0

Written by ${AUTHORS}
Licensed under AGPL3
EOF
}

die() {
    local -r _message="${1}"
    echo "${RED}${_message}${RESET}" 1>&2
    exit 1
}

validate_dependencies() {
    local _command
    for _command in "${REQUIRED_COMMANDS[@]}"; do
        if ! type "${_command}" &>/dev/null; then
            die "${_command} is not installed. Exiting."
        fi
    done
}

trim() {
    local _string="${1}"

    echo "${_string}" | xargs
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

is_root() {
    [[ ${EUID} -eq 0 ]]
}

format_floating_point() {
    local -ri _decimals_number="${1}"
    local -r _floating_point_number="${2}"

    printf "%.${_decimals_number}f" "${_floating_point_number}"
}

main() {
    validate_dependencies
}

while getopts ':hV-:' OPTION; do
    case "${OPTION}" in
        V)
            version
            exit 0
            ;;
        h)
            usage
            exit 0
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

shift "${OPTIND}"
main "${@}"
