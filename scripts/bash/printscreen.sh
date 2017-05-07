#!/bin/sh

if [ -z "$PRINTSCREEN_DIRECTORY" ]; then
    export PRINTSCREEN_DIRECTORY="$HOME/Pictures"
fi

mkdir -p "$PRINTSCREEN_DIRECTORY"
import -window root "$PRINTSCREEN_DIRECTORY/screenshot_$(date '+%Y-%m-%d-%H:%M:%S').png" 
