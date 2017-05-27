#!/usr/bin/bash

if [ -z "$PRINTSCREEN_DIRECTORY" ]; then
    export PRINTSCREEN_DIRECTORY="$HOME/Pictures"
fi

take_full_screen_picture() {
    mkdir -p "$PRINTSCREEN_DIRECTORY"
    import -window root "$PRINTSCREEN_DIRECTORY/screenshot_$(date '+%Y-%m-%d-%H:%M:%S').png" 
}

take_selected_region_picture() {
    mkdir -p "$PRINTSCREEN_DIRECTORY"
    import "$PRINTSCREEN_DIRECTORY/screenshot_$(date '+%Y-%m-%d-%H:%M:%S').png"
}

case "$1" in
    -f|--full-screen)
        take_full_screen_picture
        ;;
    -s|--selected)
        take_selected_region_picture
        ;;
    *)
        echo "Could not parse $1 option."
esac
