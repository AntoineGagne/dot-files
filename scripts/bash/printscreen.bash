#!/usr/bin/bash

if [ -z "${printscreen_directory}" ]; then
    declare -rx printscreen_directory="$HOME/Pictures"
fi

declare -r screenshot_name="${printscreen_directory}/screenshot_$(date '+%Y-%m-%d-%H:%M:%S').png"
declare -ri expire_time=200
declare -r application_name="$(basename "${0}")"

take_full_screen_picture() {
    mkdir -p "${printscreen_directory}"
    import -window root "${screenshot_name}"

    send_printscreen_as_notification
}

take_selected_region_picture() {
    mkdir -p "${printscreen_directory}"
    import "${screenshot_name}"

    send_printscreen_as_notification
}

take_current_screen_picture() {
    active_window_id="$(xprop -root | awk '/^_NET_ACTIVE_WINDOW\(WINDOW\)/ {print $5}')"
    import -window "${active_window_id}" "${screenshot_name}"

    send_printscreen_as_notification
}

send_printscreen_as_notification() {
    if ! type "notify-send" &>/dev/null || [[ "$(pgrep -c 'dunst')" -lt 1 ]]; then
        return 1
    fi

    notify-send --urgency=low \
                --expire-time=${expire_time} \
                --app-name="${application_name}" \
                --icon="${screenshot_name}" \
                "Capture"
}

case "$1" in
    -a|--all-windows)
        take_full_screen_picture
        ;;
    -s|--selected-region)
        take_selected_region_picture
        ;;
    -c|--current-window)
        take_current_screen_picture
        ;;
    *)
        echo "Could not parse $1 option."
        exit 1
esac
