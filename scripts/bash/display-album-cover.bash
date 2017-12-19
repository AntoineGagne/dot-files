#! /usr/bin/env bash

declare -r MUSIC_DIRECTORY="${HOME}/Music"
declare -r TEMPORARY_DIRECTORY="/tmp"
declare -r COVER="${TEMPORARY_DIRECTORY}/cover.png"

reset_background() {
    rm "${COVER}"
    kitty icat --clear
}

display_album() {
    local -r _file="${MUSIC_DIRECTORY}/$(mpc --format %file% current)"
    local -r _temporary_file="${TEMPORARY_DIRECTORY}/$(uuidgen --random).png"
    local -r _column_width="$(tput cols)"
    local -r _line_height="$(tput lines)"

    if [[ ! -a "${_file}" ]]; then
        reset_background
        exit 1
    fi

    if ! ffmpeg -8 -i "${_file}" -an -vcodec copy "${_temporary_file}" &>/dev/null; then
        reset_background
        exit 1
    fi

    # resize the image's width to 300px 
    if ! convert -quiet "${_temporary_file}" -resize 150x100 "${COVER}" &>/dev/null; then
        reset_background
        exit 1
    fi

    if [[ ! -f "${COVER}" ]] ; then
        reset_background
        exit 1
    fi

    local -ri _x=$(printf "%.0f" "$(echo "${_column_width} * 0.10" | bc)")
    local -ri _y=$(printf "%.0f" "$(echo "${_line_height} * 0.001" | bc)")
    kitty icat --align="left" --place="30x30@${_x}x${_y}" "${COVER}"
}

display_album 2>>"${HOME}/.ncmpcpp/cover-display.log"
