#!/usr/bin/bash

toggle_sound() {
    pactl set-sink-mute @DEFAULT_SINK@ toggle
}

control_volume() {
    pactl set-sink-mute @DEFAULT_SINK@ false
    pactl set-sink-volume @DEFAULT_SINK@ $1
}

case "$1" in
    -t|--toggle)
        toggle_sound
        ;;
    -c|--control)
        control_volume "$2"
        ;;
    *)
        echo "Could not parse $1 option"
        ;;
esac
