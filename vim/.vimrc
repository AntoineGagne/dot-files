filetype off         " required for Vundle

" {{{1 Python Hosts

" if has('nvim')
"     if isdirectory(expand('~/.virtualenvs/neovim2/bin/'))
"         let g:python_host_prog = expand('~/.virtualenvs/neovim2/bin/python')
"     endif
"     if isdirectory(expand('~/.virtualenvs/neovim3/bin/'))
"         let g:python3_host_prog = expand('~/.virtualenvs/neovim3/bin/python')
"     endif
" endif

" {{{1 Plugins

call plug#begin('~/.vim/bundle')

    " {{{2 Typescript syntax highlighting
    Plug 'HerringtonDarkholme/yats.vim'

    " {{{2 Autocompletion plugin
    Plug 'neovim/nvim-lsp'
    Plug 'Shougo/deoplete-lsp'
    " Plug 'nvim-lua/completion-nvim'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'deoplete-plugins/deoplete-lsp'

    " Autoformatting
    Plug 'editorconfig/editorconfig-vim'
    Plug 'kana/vim-operator-user'
    Plug 'rhysd/vim-clang-format', { 'for': ['c', 'cpp'] }

    " Tree Sitter
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

    " {{{2 Vimscript Language
    Plug 'junegunn/vader.vim', { 'for': ['vim'] }

    " {{{2 Elixir Language
    Plug 'elixir-editors/vim-elixir'

    " {{{2 Python Language
    Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}

    " {{{2 Surrounding Text Objects
    Plug 'tpope/vim-surround'

    " {{{2 Transformation
    Plug 'tpope/vim-abolish'

    " {{{2 Status bar
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    "  {{{2 Syntax highlighting for the Haskell language
    Plug 'neovimhaskell/haskell-vim', { 'for': ['haskell'] }

    " {{{2 Perfect language syntax highlighting
    Plug 'AntoineGagne/perfect-language-vim'

    " {{{2 HTML5 Syntax Highlighting
    Plug 'othree/html5.vim', { 'for': 'html' }

    " {{{2 JS & JSX
    Plug 'pangloss/vim-javascript', {'for': 'javascript'}
    Plug 'mxw/vim-jsx', {'for': 'javascript'}

    " {{{2 Undo Tree Visualization
    Plug 'mbbill/undotree'

    " {{{2 Syntax checking
    Plug 'w0rp/ale'

    " {{{2 Asynchronous Execution
    Plug 'Shougo/vimproc.vim', { 'do': 'make' }

    " {{{2 Elm Syntax Highlighting
    Plug 'andys8/vim-elm-syntax', { 'for': 'elm' }

    " {{{2 Pandoc Syntax
    Plug 'vim-pandoc/vim-pandoc', { 'for': ['markdown', 'pandoc'] }
    Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': ['markdown', 'pandoc'] }

    " {{{2 Markdown Tables
    Plug 'dhruvasagar/vim-table-mode', { 'for': ['markdown', 'rst', 'pandoc'] }

    " {{{2 TOML Support
    Plug 'cespare/vim-toml'

    " {{{2 ColorScheme 
    if has('nvim')
        Plug 'luisiacc/gruvbox-baby', {'branch': 'main'}
    else
        Plug 'morhetz/gruvbox'
    endif

    " {{{2 Snippets
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'

    " {{{2 Note Taking
    Plug 'vimwiki/vimwiki'

    " {{{2 Navigation
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'

    " {{{2 jq language
    Plug 'vito-c/jq.vim'

    " {{{2 Others
    Plug 'tpope/vim-repeat'
    
    " {{{2 Purescript
    Plug 'raichoo/purescript-vim'

    " {{{2 Dhall
    Plug 'vmchale/dhall-vim'

    " {{{2 Bitbake language
    Plug 'kergoth/vim-bitbake'

    " {{{2 CSS language
    Plug 'hail2u/vim-css3-syntax'
call plug#end()
filetype plugin indent on

" {{{1 OCaml Required Commands
" These can't be put into autoloaded files otherwise it doesn't work
source ~/.vim/runtime/ocaml.vim
