#!/usr/bin/bash

named_volume_pipes="${HOME}/.volume*"

toggle_sound() {
    pactl set-sink-mute @DEFAULT_SINK@ toggle
    write_volume_status
}

control_volume() {
    pactl set-sink-mute @DEFAULT_SINK@ false
    pactl set-sink-volume @DEFAULT_SINK@ "$1"
    write_volume_status
}

write_volume_status() {
    for named_volume_pipe in $named_volume_pipes; do
        if [ "$(is_muted)" = "no" ]; then
            get_volume_level >"${named_volume_pipe}" &
        else
            echo "Muted" >"${named_volume_pipe}" &
        fi
    done
}

is_muted() {
    export sink_number
    sink_number="$(get_default_sink_number)"
    pactl list sinks | perl -000ne 'if(/#$ENV{'sink_number'}/){/(Mute:\s*(.*)\s*)/; print "$2\n"}'
}

get_volume_level() {
    export sink_number
    sink_number="$(get_default_sink_number)"
    pactl list sinks | perl -000ne 'if(/#$ENV{'sink_number'}/){/Volume:\s*front-left:.*\/\s*(\d+)%.*front-right:.*\/\s*(\d+)%.*/; print "$1 $2\n"}' | awk '{print ($1 + $2) / 2 "%"}'
}

get_default_sink_number() {
    pacmd list-sinks | perl -000ne 'if (/\*\s*index/){/(\*\s*index:\s*(\d+))/; print "$2\n"}'
}

case "$1" in
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
        echo "Could not parse $1 option."
        exit 1
        ;;
esac
