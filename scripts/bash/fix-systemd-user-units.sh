#! /bin/sh

systemd_units_directory="${HOME}/.config/systemd/user"

if [ ! -f "${systemd_units_directory}/$(basename "$1")" ]; then
    # Swap all the `~` for the absolute `${HOME}` path
    sed -i 's@~@'"${HOME}"'@g@' "${1}"
    cp "${1}" "${systemd_units_directory}/"
fi
