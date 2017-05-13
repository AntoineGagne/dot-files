#! /bin/sh

temporary_directory="${HOME}/.tmp"

if [ -d "${temporary_directory}" ]; then
    /usr/bin/find "${temporary_directory}" -name '*~' -type f -delete
fi
