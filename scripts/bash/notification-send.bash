#! /usr/bin/env bash

can_send_notifications() {
    type "notify-send" &>/dev/null && [[ "$(pgrep -c 'dunst')" -ge 1 ]]
}

main() {
    if ! can_send_notifications; then
        return 1
    fi

    notify-send "${@}"
}

main "${@}"
