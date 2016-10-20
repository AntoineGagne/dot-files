#! /bin/sh

sudo pip3 install virtualenv \
    && sudo pip3 install virtualenvwrapper \
    && mkdir "${HOME}/.virtualenvs" \
    && mkvirtualenv neovim2 -p /usr/bin/python2 \
    && pip install neovim \
    && mkvirtualenv neovim3 -p /usr/bin/python3 \
    && pip install neovim \
    && deactivate
