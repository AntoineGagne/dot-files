#! /bin/sh

# Clone the `Vundle` plugin manager
mkdir -p "${HOME}/.vim/bundle" \
    && ln -sf "$(pwd)/colors" "${HOME}/.vim" \
    && git clone git@github.com:VundleVim/Vundle.vim.git "${HOME}/.vim/bundle"

# Setup `Neovim`
mkdir -p "${HOME}/.config" \
    && ln -sf "${HOME}/.vim" "${HOME}/.config/nvim" \
    && ln -sf "${HOME}/.vimrc" "${HOME}/.config/nvim/init.vim"

# Install the plugins
nvim +PluginInstall +qa \
    && cd ~/.vim/bundle/vimproc.vim \
    && make
