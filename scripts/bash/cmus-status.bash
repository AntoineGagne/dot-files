#!/usr/bin/bash

if type "cmus-parse" >/dev/null 2>&1; then
    cmus-parse "$1" "$2" > "$HOME/.song-information" &
fi
