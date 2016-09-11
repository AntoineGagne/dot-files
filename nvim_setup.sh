#! /bin/sh

# Clone the `Vundle` plugin manager
mkdir -p ~/.vim/bundle \
&& ln -sf "$(pwd)/colors" "~/.vim" \
&& git clone git@github.com:VundleVim/Vundle.vim.git "~/.vim/bundle"

# Setup `Neovim`
mkdir -p "~/.config" \
&& ln -s "~/.vim ~/.config/nvim" \
&& ln -s "~/.vimrc ~/.config/nvim/init.vim"
