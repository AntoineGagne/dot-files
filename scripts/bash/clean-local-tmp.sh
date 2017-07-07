#! /bin/sh

directories="${HOME}/.vim/.tmp ${HOME}/.vim/.undo ${HOME}/.vim/.swap"

for directory in $directories; do
    if [ -d "${directory}" ]; then
        /usr/bin/find "${directory}" -name '*' -type f -mtime +90 -delete
    fi
done
