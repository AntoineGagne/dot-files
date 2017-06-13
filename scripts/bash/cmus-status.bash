#!/usr/bin/bash

named_song_pipes="${HOME}/.song-information*"
if type "cmus-parse" >/dev/null 2>&1; then
    for named_song_pipe in $named_song_pipes; do
        cmus-parse "$1" "$2" > "$named_song_pipe" &
    done
fi
