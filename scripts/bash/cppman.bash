#! /usr/bin/env bash

main() {
    local -r _subject="${1}"

    xdg-open "http://en.cppreference.com/mwiki/index.php?search=${_subject}"
}

main "${@}"
