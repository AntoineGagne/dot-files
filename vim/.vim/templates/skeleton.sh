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

AUTHORS='Antoine Gagné'
PROGRAM_NAME="$(basename "${0%.*}")"
REQUIRED_COMMANDS=""

usage() {
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
${PROGRAM_NAME} v0.0.0

Written by ${AUTHORS}
Licensed under AGPL3
EOF
}

die() {
    _message="${1}"
    echo "${_message}" 1>&2
    exit 1
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

if [ -n "${*}" ]; then
    shift "${OPTIND}"
fi
main "${@}"
