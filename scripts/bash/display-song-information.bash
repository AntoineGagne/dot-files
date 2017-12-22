#! /usr/bin/env bash

declare -r music_directory="${HOME}/Music"
declare -r temporary_directory="/tmp"
declare -r cover="${temporary_directory}/cover.png"

declare -r application_name="ncmpcpp"
declare -ri expire_time=1000

display_album() {
    local -r _file="${music_directory}/$(mpc --format %file% current)"
    local -r _temporary_file="${temporary_directory}/$(uuidgen --random).png"

    rm -f "${cover}"

    if [[ ! -a "${_file}" ]]; then
        exit 1
    fi

    if ! ffmpeg -i "${_file}" -an -vcodec copy "${_temporary_file}" &>/dev/null; then
        exit 1
    fi

    # resize the image's width to 300px 
    if ! convert -quiet "${_temporary_file}" -resize 128x128 "${cover}" &>/dev/null; then
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
        notify-send --urgency=low \
                    --expire-time=${expire_time} \
                    --icon="${cover}" \
                    --app-name="${application_name} - ${_player_status~}" \
                    "${_album_information}" \
                    "${_song_information}"
    else
        notify-send --urgency=low \
                    --expire-time=${expire_time} \
                    --app-name="${application_name} - ${_player_status~}" \
                    "${_album_information}" \
                    "${_song_information}"
    fi
}

main() {
    if ! type "notify-send" &>/dev/null || [[ "$(pgrep -c 'dunst')" -lt 1 ]]; then
        exit 1
    fi

    display_current_mpd_status
}

main
