#! /usr/bin/env bash

declare -r base_directory="/sys/class/backlight"
declare -r handler="${base_directory}/$(ls ${base_directory})"
declare -r max_brightness=$(cat "${handler}/max_brightness")
declare -r current_brightness=$(cat "${handler}/brightness")
declare -r application_name="$(basename ${0})"
declare -ri expire_time=1000

calculate_new_brightness() {
    local -r _brightness_percentage_change="${1}"
    local -r _current_brightness_percentage=$(( 100 * current_brightness / max_brightness ))
    local -r _new_brightness_percentage=$((_current_brightness_percentage + _brightness_percentage_change))
    echo $(( max_brightness * _new_brightness_percentage / 100 ))
}

main() {
    local -r _brightness_percentage_change="${1}"
    local -r _new_brightness="$(calculate_new_brightness "${_brightness_percentage_change}")"
    local -r _adjusted_brightness="$(adjust_new_brightness "${_new_brightness}")"

    set_brightness "${_adjusted_brightness}" "${handler}/brightness"
    send_brightness_level_as_notification "${_adjusted_brightness}"
}

set_brightness() {
    local -r _new_brightness="${1}"
    local -r _brightness_file="${2}"

    echo "${_new_brightness}" | sudo tee "${_brightness_file}"
}

adjust_new_brightness() {
    local -r _new_brightness="${1}"
    local _adjusted_brightness

    if [ "${_new_brightness}" -le "${max_brightness}" ] && [ "${_new_brightness}" -ge 0 ]; then
        _adjusted_brightness="${_new_brightness}"
    elif [ "${_new_brightness}" -lt 0 ]; then
        _adjusted_brightness="0"
    else
        _adjusted_brightness="${max_brightness}"
    fi

    echo "${_adjusted_brightness}"
}

send_brightness_level_as_notification() {
    local -r _new_brightness="${1}"
    local -r _new_brightness_percentage=$(( _new_brightness * 100 / max_brightness ))

    notification-send --urgency=low \
                      --expire-time=${expire_time} \
                      --app-name="${application_name}" \
                      --hint="int:value:${_new_brightness_percentage}" "Brightness"
}

main "${@}"
