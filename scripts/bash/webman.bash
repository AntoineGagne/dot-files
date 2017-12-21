#! /usr/bin/env bash

main() {
    local _topic
    local _subject

    while getopts "s:t:h" options "${@}"; do
        case "${options}" in
            t)
                _topic="${OPTARG}"
                ;;
            s)
                _subject="${OPTARG}"
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

    search_mdn "${_subject}" "${_topic}"
}

search_mdn() {
    local -r _subject="${1}"
    local -r _topic="${2}"

    xdg-open "https://developer.mozilla.org/en-US/search?q=${_subject}&topic=${_topic}"
}

usage() {
    cat << EOF
Usage: jsman [-t TOPIC] [-s SUBJECT] [-h]

Available options:
  -t TOPIC    The topic to search for (i.e. 'js', 'html', 'css')
  -s SUBJECT  The subject to search for
  -h          Display this message
EOF
}

main "${@}"
