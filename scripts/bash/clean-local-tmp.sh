#! /bin/sh

directories="${HOME}/.tmp ${HOME}/.undo ${HOME}/.swap"

for directory in $directories; do
    if [ -d "${directory}" ]; then
        /usr/bin/find "${directory}" -name '*' -type f -mtime +90 -delete
    fi
done
