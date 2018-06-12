filetype off         " required for Vundle
set nocompatible " no longer compatible with Vi, required for Vundle

" {{{1 Python Hosts

if has('nvim')
    if isdirectory(expand('~/.virtualenvs/neovim2/bin/'))
        let g:python_host_prog = expand('~/.virtualenvs/neovim2/bin/python')
    endif
    if isdirectory(expand('~/.virtualenvs/neovim3/bin/'))
        let g:python3_host_prog = expand('~/.virtualenvs/neovim3/bin/python')
    endif
endif

" {{{1 Plugins

call plug#begin('~/.vim/bundle')
    " {{{2 Autocompletion plugin
    " Plug 'Valloric/YouCompleteMe'
    Plug 'autozimu/LanguageClient-neovim', {
                \    'branch': 'next',
                \    'do': './install.sh'
                \ }
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

    Plug 'editorconfig/editorconfig-vim'

    " {{{2 Erlang Language
    Plug 'vim-erlang/vim-erlang-runtime'
    Plug 'vim-erlang/vim-erlang-compiler'
    Plug 'vim-erlang/vim-erlang-tags'
    Plug 'vim-erlang/vim-erlang-omnicomplete'

    " {{{2 Tags Management
    Plug 'ludovicchabant/vim-gutentags'

    " {{{2 Surrounding Text Objects
    Plug 'tpope/vim-surround'

    " {{{2 Status bar
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    " {{{2 Rust autocompletion bundle
    Plug 'racer-rust/vim-racer'

    "  {{{2 Syntax highlighting for the Haskell language
    Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

    " {{{2 Perfect language syntax highlighting
    Plug 'AntoineGagne/perfect-language-vim'

    " {{{2 HTML5 Syntax Highlighting
    Plug 'othree/html5.vim', { 'for': 'html' }

    " {{{2 Syntax checking
    Plug 'w0rp/ale'

    " {{{2 Typescript syntax highlighting
    Plug 'leafgarland/typescript-vim'

    " {{{2 Asynchronous Execution
    Plug 'Shougo/vimproc.vim', { 'do': 'make' }

    " {{{2 Elm Syntax Highlighting
    Plug 'elmcast/elm-vim'

    " {{{2 Pandoc Syntax
    Plug 'vim-pandoc/vim-pandoc', { 'for': 'markdown' }
    Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': 'markdown' }

    " {{{2 Markdown Tables
    Plug 'dhruvasagar/vim-table-mode', { 'for': ['markdown', 'rst'] }

    " {{{2 ColorScheme 
    Plug 'morhetz/gruvbox'

    " {{{2 Snippets
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'

    " {{{2 Note Taking
    Plug 'vimwiki/vimwiki'
call plug#end()
filetype plugin indent on

" {{{1 Configuration Files
source ~/.vim/startup/settings.vim
source ~/.vim/startup/mappings.vim
