#! /bin/sh

# Install the plugins
nvim +PluginInstall +qa \
    && cd ~/.vim/bundle/vimproc.vim \
    && make
