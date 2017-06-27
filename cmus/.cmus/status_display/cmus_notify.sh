#! /bin/sh

if type "cmus_notify" > /dev/null; then
    cmus_notify "$*" &
fi

if type "cmus-status" > /dev/null; then
    cmus-status '<fn=1></fn> {title} by {artist}' "$*" &
fi
