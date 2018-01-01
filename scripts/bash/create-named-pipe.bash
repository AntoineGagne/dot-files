#!/usr/bin/env bash

PIPE_PATH="$1"
DEFAULT_VALUE="$2"

if [ -p "$PIPE_PATH" ]; then
    rm "$PIPE_PATH"
fi

mkfifo "$PIPE_PATH"
echo "$DEFAULT_VALUE" > "$PIPE_PATH" &
