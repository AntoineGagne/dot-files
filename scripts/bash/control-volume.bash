#!/usr/bin/bash

declare -r named_volume_pipes="${HOME}/.volume*"
declare -ri expire_time=200
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
    write_volume_to_named_pipes "${_volume_level}" "${_is_muted}"
    send_volume_level_as_notification "${_volume_level}" "${_is_muted}"
}

write_volume_to_named_pipes() {
    local -r _volume_level="${1}"
    local -r _is_muted="${2}"
    for named_volume_pipe in $named_volume_pipes; do
        if [ "${_is_muted}" = "no" ]; then
            echo "${_volume_level}" >"${named_volume_pipe}" &
        else
            echo "Muted" >"${named_volume_pipe}" &
        fi
    done
}

send_volume_level_as_notification() {
    local -r _volume_level="${1}"
    local -r _is_muted="${2}"

    if ! type "notify-send" &>/dev/null || [[ "$(pgrep -c 'dunst')" -lt 1 ]]; then
        return 1
    fi

    if [ "${_is_muted}" = "no" ]; then
        notify-send --urgency=low \
                    --expire-time=${expire_time} \
                    --app-name="${application_name}" \
                    --icon="${HOME}/.icons/volume/volume-medium.png" \
                    --hint="int:value:${_volume_level}" "Volume"
    else
        notify-send --urgency=low \
                    --expire-time=${expire_time} \
                    --icon="${HOME}/.icons/volume/volume-muted.png" \
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
