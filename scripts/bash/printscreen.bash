#!/usr/bin/bash

if [ -z "$PRINTSCREEN_DIRECTORY" ]; then
    export PRINTSCREEN_DIRECTORY="$HOME/Pictures"
fi

take_full_screen_picture() {
    mkdir -p "$PRINTSCREEN_DIRECTORY"
    import -window root "$(generate_screenshot_name)"
}

take_selected_region_picture() {
    mkdir -p "$PRINTSCREEN_DIRECTORY"
    import "$(generate_screenshot_name)"
}

take_current_screen_picture() {
    active_window_id="$(xprop -root | awk '/^_NET_ACTIVE_WINDOW\(WINDOW\)/ {print $5}')"
    import -window "${active_window_id}" "$(generate_screenshot_name)"
}

generate_screenshot_name() {
    echo "$PRINTSCREEN_DIRECTORY/screenshot_$(date '+%Y-%m-%d-%H:%M:%S').png"
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
