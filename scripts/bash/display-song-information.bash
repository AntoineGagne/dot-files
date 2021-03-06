#! /usr/bin/env bash

declare -r music_directory="${HOME}/Music"
declare -r temporary_directory="/tmp"
declare -r cover="${temporary_directory}/cover.png"
declare -r temporary_file="${temporary_directory}/$(uuidgen --random).png"

declare -r application_name="ncmpcpp"
declare -ri expire_time=1000

display_album() {
    local -r _file="${music_directory}/$(mpc --format %file% current)"

    rm -f "${cover}"

    if [[ ! -a "${_file}" ]]; then
        exit 1
    fi

    if ! ffmpeg -i "${_file}" -an -vcodec copy "${temporary_file}" &>/dev/null; then
        exit 1
    fi

    # resize the image's width to 300px 
    if ! convert -quiet "${temporary_file}" -resize 128x128 "${cover}" &>/dev/null; then
        exit 1
    fi

    if [[ ! -f "${cover}" ]] ; then
        exit 1
    fi
}

display_current_mpd_status() {
    local -r _current_player_information="$(mpc --format '%album% [(%date%)]\n%artist% - %title% [(%time%)]')"
    local -r _player_status="$(echo "${_current_player_information}" | awk 'NR == 3 {print $1}' | sed -e 's@^\[\(.*\)\]@\1@')"
    local -r _album_information="$(echo "${_current_player_information}" | awk 'NR == 1 {print $0}')"
    local -r _song_information="$(echo "${_current_player_information}" | awk 'NR == 2 {print $0}')"
    local -r _has_album_cover="$(display_album)"

    if [[ "${_has_album_cover}" -ne 1 ]]; then
        notification-send --urgency=low \
                          --expire-time=${expire_time} \
                          --icon="${cover}" \
                          --app-name="${application_name} - ${_player_status~}" \
                          "${_album_information}" \
                          "${_song_information}"
    else
        notification-send --urgency=low \
                          --expire-time=${expire_time} \
                          --app-name="${application_name} - ${_player_status~}" \
                          "${_album_information}" \
                          "${_song_information}"
    fi
}

clean_up() {
    rm -f "${temporary_file}"
}

main() {
    display_current_mpd_status
}

main
trap 'clean_up' EXIT
