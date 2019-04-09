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
declare -ra REQUIRED_COMMANDS=(az)

declare AZURE_SUBSCRIPTION_ID
declare KUBERNETES_NAME
declare KUBERNETES_RESOURCE_GROUP

repeat() {
    local -r _characters="${1}"
    local -ri _times="${2}"

    yes "${_characters}" | head -n "${_times}" | tr -d '\n'
}

usage() {
    local -r _padding="$(repeat ' ' "${#PROGRAM_NAME}")"
    cat <<- EOF
Usage: ${PROGRAM_NAME} [-h|--help] [-V|--version]
       ${_padding} [-n NAME|--name=NAME]
       ${_padding} [-s SUBSCRIPION_ID|--subscription-id=SUBSCRIPION_ID]
       ${_padding} [-g RESOURCE_GROUP|--resource-group=RESOURCE_GROUP]
Display the corresponding Kubernetes dashboard.
Example: ${PROGRAM_NAME} --name=kube \\
         ${_padding} --resource-group=Kubernetes \\
         ${_padding} --subscription-id=aeb4d26d-e1e3-4604-a811-ddaa335b9b35

Available options:
  -g RESOURCE_GROUP, --resource-group=RESOURCE_GROUP    the corresponding resource group
  -h, --help                                            display this help text and exit
  -n NAME, --name=NAME                                  the corresponding name
  -s SUBSCRIPION_ID, --subscription-id=SUBSCRIPION_ID   the corresponding subscription ID
  -V, --version                                         display version information and exit
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
    local -r _message="${1}"
    echo "${RED}${_message}${RESET}"
    exit 1
}

validate_dependencies() {
    local _command
    for _command in "${REQUIRED_COMMANDS[@]}"; do
        if ! is_program_installed "${_command}"; then
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

login_to_azure() {
    local -r _subscription_id="${1}"
    az account set -s "${_subscription_id}"
}

open_kubernetes_dashboard() {
    local -r _resource_group="${1}"
    local -r _name="${2}"

    az aks get-credentials --resource-group "${_resource_group}" --name "${_name}"
    nohup az aks browse --resource-group "${_resource_group}" --name "${_name}" &
}

main() {
    validate_dependencies
    login_to_azure "${AZURE_SUBSCRIPTION_ID}"
    open_kubernetes_dashboard "${KUBERNETES_RESOURCE_GROUP}" "${KUBERNETES_NAME}"
}

while getopts ':hV-:g:n:s:' OPTION; do
    case "${OPTION}" in
        V)
            version
            exit 0
            ;;
        h)
            usage
            exit 0
            ;;
        s)
            AZURE_SUBSCRIPTION_ID="${OPTARG}"
            ;;
        n)
            KUBERNETES_NAME="${OPTARG}"
            ;;
        g)
            KUBERNETES_RESOURCE_GROUP="${OPTARG}"
            ;;
        -)
            case "${OPTARG}" in
                help)
                    usage
                    exit 0
                    ;;
                name)
                    die "Option '--name' requires one argument."
                    ;;
                name=)
                    die "Option '--name' requires one argument."
                    ;;
                name=?*)
                    KUBERNETES_NAME="${OPTARG#*=}"
                    ;;
                resource-group)
                    die "Option '--resource-group' requires one argument."
                    ;;
                resource-group=)
                    die "Option '--resource-group' requires one argument."
                    ;;
                resource-group=?*)
                    KUBERNETES_RESOURCE_GROUP="${OPTARG#*=}"
                    ;;
                subscription-id)
                    die "Option '--subscription-id' requires one argument."
                    ;;
                subscription-id=)
                    die "Option '--subscription-id' requires one argument."
                    ;;
                subscription-id=?*)
                    AZURE_SUBSCRIPTION_ID="${OPTARG#*=}"
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
