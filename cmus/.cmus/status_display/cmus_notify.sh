#! /usr/bin/env bash

if type "cmus_notify" > /dev/null; then
    output="$(cmus_notify --configuration_file ~/.cmus-notify "$*")"
    for pipe in "${HOME}/.song-information"*; do
        echo "${output}" > "${pipe}" &
    done
fi
