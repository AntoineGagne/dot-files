#!/usr/bin/bash

if type "cmus-parse" > /dev/null; then
    cmus-parse "$1" "$2" > "$HOME/.song-information" &
fi
