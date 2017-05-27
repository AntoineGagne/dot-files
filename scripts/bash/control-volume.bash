#!/usr/bin/bash

toggle_sound() {
    pactl set-sink-mute @DEFAULT_SINK@ toggle
    write_volume_status
}

control_volume() {
    pactl set-sink-mute @DEFAULT_SINK@ false
    pactl set-sink-volume @DEFAULT_SINK@ "$1"
    write_volume_status
}

create_volume_pipe() {
    if [ -p "$HOME/.volume" ]; then
        rm "$HOME/.volume"
    fi
    mkfifo "$HOME/.volume"
    write_volume_status >"$HOME/.volume" &
}

write_volume_status() {
    if [ "$(is_muted)" = "no" ]; then
        get_volume_level >"$HOME/.volume" &
    else
        echo "Muted" > "$HOME/.volume" &
    fi
}

is_muted() {
    export sink_number
    sink_number="$(get_default_sink_number)"
    pactl list sinks | perl -000ne 'if(/#$ENV{'sink_number'}/){/(Mute:\s*(.*)\s*)/; print "$2\n"}'
}

get_volume_level() {
    export sink_number
    sink_number="$(get_default_sink_number)"
    pactl list sinks | perl -000ne 'if(/#$ENV{'sink_number'}/){/Volume:\s*front-left:.*\/\s*(\d+%).*front-right:.*\/\s*(\d+%).*/; print "$1 $2\n"}'
}

get_default_sink_number() {
    pacmd list-sinks | perl -000ne 'if (/\*\s*index/){/(\*\s*index:\s*(\d+))/; print "$2\n"}'
}

case "$1" in
    --create-pipe)
        create_volume_pipe
        ;;
    -v|--volume)
        get_volume_level
        ;;
    -d|--default-sink)
        get_default_sink_number
        ;;
    -m|--muted)
        is_muted
        ;;
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
