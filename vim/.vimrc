filetype off         " required for Vundle
set nocompatible " no longer compatible with Vi, required for Vundle

" {{{1 Python Hosts

if has('nvim')
    if isdirectory(expand('~') . '/.virtualenvs/neovim2/bin/')
        let g:python_host_prog = expand('~') . '/.virtualenvs/neovim2/bin/python'
    endif
    if isdirectory(expand('~') . '/.virtualenvs/neovim3/bin/')
        let g:python3_host_prog = expand('~') . '/.virtualenvs/neovim3/bin/python'
    endif
endif

" {{{1 Plugins

if !has('nvim')
    " Set the runtime path to include Vundle and initialize
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()
        " Alternatively, pass a path where Vundle should install plugins
        " Call vundle#begin('~/some/path/here')

        " Let Vundle manage Vundle, required
        Plugin 'VundleVim/Vundle.vim'

        " {{{2 Autocompletion plugin
        Plugin 'Valloric/YouCompleteMe'
        " Plugin 'Shougo/neocomplete.vim'

        " {{{2 Surrounding Text Objects
        Plugin 'tpope/vim-surround'

        " {{{2 Status bar
        Plugin 'vim-airline/vim-airline'
        Plugin 'vim-airline/vim-airline-themes'

        " {{{2 Rust autocompletion bundle
        Plugin 'racer-rust/vim-racer'

        " {{{2 Syntax highlighter for the Rust language
        Plugin 'rust-lang/rust.vim'

        " {{{2 Haskell Syntax Checking
        Plugin 'eagletmt/ghcmod-vim'

        " {{{2 Autocompletion for the Haskell language
        Plugin 'eagletmt/neco-ghc'

        "  {{{2 Syntax highlighting for the Haskell language
        Plugin 'neovimhaskell/haskell-vim'

        " {{{2 HTML5 Syntax Highlighting
        Plugin 'othree/html5.vim'

        "  {{{2 Show filesystem as a tree
        " See https://shapeshed.com/vim-netrw/
        " Plugin 'scrooloose/nerdtree'

        " {{{2 Syntax checking
        " Plugin 'vim-syntastic/syntastic'
        Plugin 'w0rp/ale'

        " {{{2 Asynchronous Execution
        Plugin 'Shougo/vimproc.vim'

        " {{{2 Elm Syntax Highlighting
        Plugin 'elmcast/elm-vim'

        " {{{2 Pandoc Syntax
        Plugin 'vim-pandoc/vim-pandoc'
        Plugin 'vim-pandoc/vim-pandoc-syntax'

        " {{{2 Markdown Tables
        Plugin 'dhruvasagar/vim-table-mode'

        " {{{2 ColorScheme 
        Plugin 'morhetz/gruvbox'

        " {{{2 Utility functions
        " Plugin 'MarcWeber/vim-addon-mw-utils'
        " Plugin 'tomtom/tlib_vim'

        " {{{2 Snippets
        " Plugin 'garbas/vim-snipmate'
        Plugin 'SirVer/ultisnips'
        Plugin 'honza/vim-snippets'

        " {{{2 Aligning
        " Plugin 'godlygeek/tabular'

        " {{{2 Haskell unicode symbols
        " Plugin 'Twinside/vim-haskellConceal'

        " {{{2 Note Taking
        Plugin 'vimwiki/vimwiki'

    " All of your Plugins must be added before the following line
    " Required
    call vundle#end()
else
    call plug#begin('~/.vim/bundle')
        " {{{2 Autocompletion plugin
        " Plug 'Valloric/YouCompleteMe'
        Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

        " {{{2 Surrounding Text Objects
        Plug 'tpope/vim-surround'

        " {{{2 Status bar
        Plug 'vim-airline/vim-airline'
        Plug 'vim-airline/vim-airline-themes'

        " {{{2 Rust autocompletion bundle
        Plug 'racer-rust/vim-racer'

        " {{{2 Syntax highlighter for the Rust language
        Plug 'rust-lang/rust.vim'

        " {{{2 Haskell Syntax Checking
        Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }

        " {{{2 Autocompletion for the Haskell language
        Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }

        "  {{{2 Syntax highlighting for the Haskell language
        Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

        " {{{2 HTML5 Syntax Highlighting
        Plug 'othree/html5.vim', { 'for': 'html' }

        "  {{{2 Show filesystem as a tree
        " See https://shapeshed.com/vim-netrw/
        " Plug 'scrooloose/nerdtree'

        " {{{2 Syntax checking
        " Plug 'vim-syntastic/syntastic'
        Plug 'w0rp/ale'

        " {{{2 Asynchronous Execution
        Plug 'Shougo/vimproc.vim', { 'do': 'make' }

        " {{{2 Elm Syntax Highlighting
        Plug 'elmcast/elm-vim'

        " {{{2 Pandoc Syntax
        Plug 'vim-pandoc/vim-pandoc', { 'for': 'markdown' }
        Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': 'markdown' }

        " {{{2 Markdown Tables
        Plug 'dhruvasagar/vim-table-mode', { 'for': 'markdown' }

        " {{{2 ColorScheme 
        Plug 'morhetz/gruvbox'

        " {{{2 Utility functions
        " Plug 'MarcWeber/vim-addon-mw-utils'
        " Plug 'tomtom/tlib_vim'

        " {{{2 Snippets
        " Plug 'garbas/vim-snipmate'
        Plug 'SirVer/ultisnips'
        Plug 'honza/vim-snippets'

        " {{{2 Aligning
        " Plug 'godlygeek/tabular'

        " {{{2 Haskell unicode symbols
        " Plug 'Twinside/vim-haskellConceal'

        " {{{2 Note Taking
        Plug 'vimwiki/vimwiki'
    call plug#end()
endif
filetype plugin indent on

" {{{1 Configuration Files
source ~/.vim/startup/autocommands.vim
source ~/.vim/startup/commands.vim
source ~/.vim/startup/functions.vim
source ~/.vim/startup/settings.vim
source ~/.vim/startup/mappings.vim
