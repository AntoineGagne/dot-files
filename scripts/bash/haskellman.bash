#! /usr/bin/env bash

main() {
    local -r _subject="${1}"

    xdg-open "https://www.haskell.org/hoogle/?hoogle=${_subject}"
}

main "${@}"
