#! /bin/sh

mkdir -p ~/.config \
&& ln -s ~/.vim ~/.config/nvim \
&& ln -s ~/.vimrc ~/.config/nvim/init.vim
