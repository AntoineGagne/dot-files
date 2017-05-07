#! /bin/sh

if type "cmus_notify" > /dev/null; then
    cmus_notify "$*" &
fi

if type "cmus-status" > /dev/null; then
    cmus-status '{title} by {artist}' "$*" &
fi
