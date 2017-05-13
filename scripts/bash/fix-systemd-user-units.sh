#! /bin/sh

systemd_units_directory="${HOME}/.config/systemd/user"

for file_path in "$@"; do
    file="$(basename "${file_path}")"
    if [ ! -f "${systemd_units_directory}/${file}" ]; then
        # Swap all the `~` for the absolute `${HOME}` path
        cp "${file_path}" "${systemd_units_directory}/${file}"
        sed -i 's@~@'"${HOME}"'@g@' "${systemd_units_directory}/${file}"
    fi
done
