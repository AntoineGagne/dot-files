filetype off         " required for Vundle

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

    " {{{2 Typescript syntax highlighting
    Plug 'HerringtonDarkholme/yats.vim'
    " Plug 'mhartington/nvim-typescript', {
    "             \ 'do': './install.sh', 
    "             \}

    " {{{2 Autocompletion plugin
    " Plug 'Valloric/YouCompleteMe'
    " Plug 'autozimu/LanguageClient-neovim', {
    "             \    'branch': 'next',
    "             \    'do': './install.sh'
    "             \ }
    Plug 'neovim/nvim-lsp'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

    " Autoformatting
    Plug 'editorconfig/editorconfig-vim'
    Plug 'kana/vim-operator-user'
    Plug 'rhysd/vim-clang-format', { 'for': ['c', 'cpp'] }

    " {{{2 Vimscript Language
    Plug 'junegunn/vader.vim', { 'for': ['vim'] }

    " {{{2 Erlang Language
    Plug 'vim-erlang/vim-erlang-runtime', { 'for': 'erlang' }
    Plug 'vim-erlang/vim-erlang-compiler', { 'for': 'erlang' }
    Plug 'vim-erlang/vim-erlang-omnicomplete', { 'for': 'erlang' }
    " Plug 'ppikula/vim-wrangler', { 'for': 'erlang' }
    " Plug 'vim-erlang/vim-erlang-tags', { 'for': 'erlang' }

    " {{{2 Elixir Language
    Plug 'elixir-editors/vim-elixir'

    " {{{2 Semantic Highlighting
    Plug 'arakashic/chromatica.nvim', { 'for': ['c', 'cpp'] }

    " {{{2 Python Language
    Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins', 'for': 'python'}

    " {{{2 Tags Management
    Plug 'ludovicchabant/vim-gutentags', { 'for': ['erlang'] }

    " {{{2 Surrounding Text Objects
    Plug 'tpope/vim-surround'

    " {{{2 Transformation
    Plug 'tpope/vim-abolish'

    " {{{2 Status bar
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    " {{{2 Rust autocompletion bundle
    Plug 'racer-rust/vim-racer', { 'for': 'rust' }

    "  {{{2 Syntax highlighting for the Haskell language
    Plug 'neovimhaskell/haskell-vim', { 'for': ['haskell'] }

    " {{{2 Perfect language syntax highlighting
    Plug 'AntoineGagne/perfect-language-vim'

    " {{{2 Sessions management
    Plug 'AntoineGagne/sessions-vim'

    " {{{2 HTML5 Syntax Highlighting
    Plug 'othree/html5.vim', { 'for': 'html' }

    " {{{2 JS & JSX
    Plug 'pangloss/vim-javascript', {'for': 'javascript'}
    Plug 'mxw/vim-jsx', {'for': 'javascript'}

    " {{{2 Undo Tree Visualization
    Plug 'mbbill/undotree'

    " {{{2 Syntax checking
    Plug 'w0rp/ale'

    " {{{2 C# Completion
    Plug 'OmniSharp/omnisharp-vim', {'for': 'cs'}

    " {{{2 F#
    " Plug 'fsharp/vim-fsharp', {
    "   \ 'for': 'fsharp',
    "   \ 'do':  'make fsautocomplete',
    "   \}
    Plug 'ionide/Ionide-vim', {
                \ 'do': 'make fsautocomplete',
                \ 'for': ['fsharp']
                \}

    " {{{2 Asynchronous Execution
    Plug 'Shougo/vimproc.vim', { 'do': 'make' }

    " {{{2 Elm Syntax Highlighting
    Plug 'elmcast/elm-vim', {'for': 'elm'}

    " {{{2 Pandoc Syntax
    Plug 'vim-pandoc/vim-pandoc', { 'for': ['markdown', 'pandoc'] }
    Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': ['markdown', 'pandoc'] }

    " {{{2 Markdown Tables
    Plug 'dhruvasagar/vim-table-mode', { 'for': ['markdown', 'rst', 'pandoc'] }

    " {{{2 TOML Support
    Plug 'cespare/vim-toml'

    " {{{2 ColorScheme 
    Plug 'morhetz/gruvbox'

    " {{{2 Snippets
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'

    " {{{2 Note Taking
    Plug 'vimwiki/vimwiki'

    " {{{2 Navigation
    Plug 'junegunn/fzf'
    Plug 'junegunn/fzf.vim'

    " {{{2 jq language
    Plug 'vito-c/jq.vim'

    " {{{2 Others
    Plug 'tpope/vim-repeat'

    " {{{2 Bitbake language
    Plug 'kergoth/vim-bitbake'

    " Use release branch
    " Plug 'neoclide/coc.nvim', { 'branch': 'release'}
call plug#end()
filetype plugin indent on

" {{{1 OCaml Required Commands
" These can't be put into autoloaded files otherwise it doesn't work
source ~/.vim/runtime/ocaml.vim
