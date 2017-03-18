filetype off         " required for Vundle
set nocompatible " no longer compatible with Vi, required for Vundle

" {{{1 Plugins

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
    " Alternatively, pass a path where Vundle should install plugins
    " Call vundle#begin('~/some/path/here')

    " Let Vundle manage Vundle, required
    Plugin 'gmarik/Vundle.vim'

    " {{{2 Autocompletion plugin
    Plugin 'Valloric/YouCompleteMe'
    " Plugin 'Shougo/neocomplete.vim'

    " {{{2 Status bar
    Plugin 'bling/vim-airline'
    Plugin 'vim-airline/vim-airline-themes'

    " {{{2 Rust autocompletion bundle
    Plugin 'phildawes/racer'

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
    Plugin 'scrooloose/nerdtree'

    " {{{2 Syntax checking
    Plugin 'scrooloose/syntastic'

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
    Plugin 'MarcWeber/vim-addon-mw-utils'
    Plugin 'tomtom/tlib_vim'

    " {{{2 Snippets
    Plugin 'garbas/vim-snipmate'

    " {{{2 Aligning
    Plugin 'godlygeek/tabular'

    " {{{2 Haskell unicode symbols
    Plugin 'Twinside/vim-haskellConceal'

" All of your Plugins must be added before the following line
" Required
call vundle#end()
filetype plugin indent on

" {{{1 Configuration Files
source ~/.vim/startup/autocommands.vim
source ~/.vim/startup/commands.vim
source ~/.vim/startup/functions.vim
source ~/.vim/startup/settings.vim
source ~/.vim/startup/mappings.vim
