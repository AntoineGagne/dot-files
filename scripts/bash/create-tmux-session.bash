#! /usr/bin/env bash

has-session() {
    tmux has-session -t "${1}" 2>/dev/null
}

create-session() {
    if ! has-session "${1}"; then
        tmux new-session -s "${1}" -d
    fi
}

run-command-in-first-window-of-given-session() {
    if has-session "${1}"; then
        tmux send-keys -t "${1}:1.1" C-z "${1}" Enter
    fi
}

create-session "${1}"
