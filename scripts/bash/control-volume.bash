#!/usr/bin/env bash

get_default_sink_number() {
    pacmd list-sinks | perl -000ne 'if (/\*\s*index/){/(\*\s*index:\s*(\d+))/; print "$2\n"}'
}

declare -ri expire_time=1000
declare -r application_name="$(basename "${0}")"
declare -rxi sink_number=$(get_default_sink_number)

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
    local -r _volume_level="$(get_volume_level)"
    local -r _is_muted="$(is_muted)"
    send_volume_level_as_notification "${_volume_level}" "${_is_muted}"
}

send_volume_level_as_notification() {
    local -r _volume_level="${1}"
    local -r _is_muted="${2}"

    if [ "${_is_muted}" = "no" ]; then
        notification-send --urgency=low \
                          --expire-time=${expire_time} \
                          --app-name="${application_name}" \
                          --hint="int:value:${_volume_level}" "Volume"
    else
        notification-send --urgency=low \
                          --expire-time=${expire_time} \
                          --app-name="${application_name} [Muted]" \
                          "Volume"
    fi
}

is_muted() {
    pactl list sinks | perl -000ne 'if(/#$ENV{'sink_number'}/){/(Mute:\s*(.*)\s*)/; print "$2\n"}'
}

get_volume_level() {
    pactl list sinks | perl -000ne 'if(/#$ENV{'sink_number'}/){/Volume:\s*front-left:.*\/\s*(\d+)%.*front-right:.*\/\s*(\d+)%.*/; print "$1 $2\n"}' | awk '{print ($1 + $2) / 2 "%"}'
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
