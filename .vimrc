" 1. Files
filetype off         " required for Vundle
scriptencoding utf-8 " Set the encoding to utf-8
set encoding=utf-8   " Set the encoding to utf-8

" 2. Editor
set nocompatible " no longer compatible with Vi, required for Vundle

" 3. Plugins

    let g:python_host_prog='/usr/bin/python'
    " set the runtime path to include Vundle and initialize
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()
        " alternatively, pass a path where Vundle should install plugins
        "call vundle#begin('~/some/path/here')

        " let Vundle manage Vundle, required
        Plugin 'gmarik/Vundle.vim'

        " Autocompletion plugin
        Plugin 'Valloric/YouCompleteMe'

        " Rust autocompletion bundle
        Plugin 'phildawes/racer'

        " Syntax highlighter for the Rust language
        Plugin 'rust-lang/rust.vim'

        " Autocompletion for the Haskell language
        Plugin 'eagletmt/neco-ghc'

        " Syntax highlighting for the Haskell language
        Plugin 'neovimhaskell/haskell-vim'

        " Haskell Syntax Checking
        Plugin 'eagletmt/ghcmod-vim'

        " Spell checker (requires further installations)
        " Plugin 'vim-scripts/LanguageTool'

        " HTML5 Syntax Highlighting
        Plugin 'othree/html5.vim'

        " Show filesystem as a tree
        Plugin 'scrooloose/nerdtree'

        " Syntax checking
        Plugin 'scrooloose/syntastic'

        " Asynchronous Execution
        Plugin 'Shougo/vimproc.vim'

        " Elm Syntax Highlighting
        Plugin 'elmcast/elm-vim'

        " Pandoc Syntax
        " Plugin 'vim-pandoc/vim-pandoc'
        " Plugin 'vim-pandoc/vim-pandoc-syntax'

        " Markdown Tables
        Plugin 'dhruvasagar/vim-table-mode'
        " ColorScheme 
        "Plugin 'morhetz/gruvbox'

        " Show the CSS colors visually
        " Plugin 'skammer/vim-css-color'

    " All of your Plugins must be added before the following line
    call vundle#end()            " required
    filetype plugin indent on    " required

    " YouCompleteMe options
    let g:ycm_auto_trigger = 1

    let g:ycm_register_as_syntastic_checker = 1 "default 1
    let g:Show_diagnostics_ui = 1 "default 1

    "enable neco-ghc completion from YouCompleteMe
    let g:ycm_semantic_triggers = {
                \ 'haskell' : ['.'],
                \ 'elm' : ['.']
                \}

    "will put icons in Vim's gutter on lines that have a diagnostic set.
    "Turning this off will also turn off the YcmErrorLine and YcmWarningLine
    "highlighting
    let g:ycm_enable_diagnostic_signs = 1
    let g:ycm_enable_diagnostic_highlighting = 0
    let g:ycm_always_populate_location_list = 1 "default 0
    let g:ycm_open_loclist_on_ycm_diags = 1 "default 1

    let g:ycm_complete_in_strings = 1 "default 1
    let g:ycm_collect_identifiers_from_tags_files = 0 "default 0
    let g:ycm_path_to_python_interpreter = '' "default ''

    let g:ycm_server_use_vim_stdout = 0 "default 0 (logging to console)
    let g:ycm_server_log_level = 'info' "default info

    let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'  "where to search for .ycm_extra_conf.py if not found
    let g:ycm_confirm_extra_conf = 1

    let g:ycm_goto_buffer_command = 'same-buffer' "[ 'same-buffer', 'horizontal-split', 'vertical-split', 'new-tab' ]
    let g:ycm_filetype_whitelist = { '*': 1 }
    let g:ycm_key_invoke_completion = '<C-Space>'

    nnoremap <F11> :YcmForceCompileAndDiagnostics <CR>

    " Set the omnicompleter
    set omnifunc=syntaxcomplete#Complete

    " Racer required commands
    set hidden
    let g:racer_cmd = "~/.vim/bundle/racer/target/release/racer"
    let $RUST_SRC_PATH="/home/twain/rustc-1.5.0/src/"

    " neco-ghc options
    set path=~/.cabal/bin/ghc-mod
    let g:necoghc_enable_detailed_browse = 1
    " Disable haskell-vim omnifunc
    let g:haskellmode_completion_ghc = 0
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

    " Haskell vim options
    if exists("g:loaded_haskellvim_haskell")
        " To enable highlighting of forall
        let g:haskell_enable_quantification = 1 
        " To enable highlighting of mdo and rec
        let g:haskell_enable_recursivedo = 1
        " To enable highlighting of proc
        let g:haskell_enable_arrowsyntax = 1 
        " To enable highlighting of pattern
        let g:haskell_enable_pattern_synonyms = 1 
        " To enable highlighting of type roles
        let g:haskell_enable_typeroles = 1 
        " To enable highlighting of static
        let g:haskell_enable_static_pointers = 1 

        " Indentation
        let g:haskell_indent_if = 4
        let g:haskell_indent_case = 4
        let g:haskell_indent_let = 4
        let g:haskell_indent_guard = 4
        let g:haskell_indent_where = 4
        let g:haskell_indent_do = 4
        let g:haskell_indent_in = 4
    endif

    " Haskell
    au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
    au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
    au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsInfo<CR>
    autocmd BufEnter *.hs set formatprg=pointfree

    " ghcmod-vim
    hi ghcmodType ctermbg=yellow
    let g:ghcmod_type_highlight = 'ghcmodType'

    " Syntastic
    set statusline=%f\ %h%w%m%r\ 
    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*
    set statusline+=%=%(%l,%c%V\ %=\ %P%)

    let g:syntastic_always_populate_loc_list = 1
    " Always show the errors list
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0

    " Gruvbox Settings
    "let g:gruvbox_termcolors=16
    
    " Elm
    let g:elm_jump_to_error = 0
    let g:elm_make_output_file = "elm.js"
    let g:elm_make_show_warnings = 0
    let g:elm_syntastic_show_warnings = 0
    let g:elm_browser_command = ""
    let g:elm_detailed_complete = 0
    let g:elm_format_autosave = 0
    let g:elm_setup_keybindings = 1
    let g:elm_classic_highlighting = 0

" To ignore plugin indent changes, instead use:
"filetype plugin on

" Vundle commands
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

    " Markdown table
    let g:table_mode_corner_corner="+"
    let g:table_mode_header_fillchar="="

" 4. Colors 

    " Molokai
    colorscheme molokai         " awesome colorscheme
    let g:regash256 = 1

    " Solarized
    "colorscheme solarized
    "set background=light
    "let g:solarized_termcolors=256

    " One Dark
    "syntax on
    "colorscheme onedark

    " Darcula
    "colorscheme darcula

" 5. Space & Tabs
    syntax enable                  " enable syntax processing
    set backspace=indent,eol,start " Allow backspacing over autoindent, line breaks and start of insert action
    set autoindent
    set list                       " Show spaces visually
    set listchars=space:·,tab:⟶\ 
    set tabstop=4                  " number of visual spaces per TAB
    set softtabstop=4              " number of spaces in tab when editing
    set shiftwidth=4               " number of spaces inserted for indentation
    set expandtab                  " replace tabs by spaces
    set smarttab                   " make tab insert indents instead of tabs at the beginning of a line

" 6. UI Configuration
    filetype indent on    " load filetype-specific indent files
    set cmdheight=2       " Set the command window height to 2 lines
    set confirm           " Instead of failing a command because of unsaved changes, instead raise a dialogue asking if you wish to save changed files
    set cursorline        " highligh current line
    set laststatus=2      " Always display the status line, even if only one window is displayed
    set lazyredraw        " redraw only when we need to   
    set number            " show line numbers
    set ruler             " Display the cursor position on the last line of the screen or in the status line of a window
    set showcmd           " show command in bottom bar
    set showmatch         " highlight matching [{()}]
    set visualbell        " Use visual bell instead of beeping when doing something wrong
    set wildmenu          " visual autocomplete for command menu
    "set termguicolors
    if $COLORTERM == 'gnome-terminal'
        set t_Co=256
    endif

" 7. Searching
    set ignorecase        " Case insensitive search
    set smartcase         " Case sensitive search when using capitals
    set incsearch         " search as characters are entered
    set hlsearch          " highlight matches

" 8. Folding
    set foldenable          " enable folding
    set foldlevelstart=10   " open most folds by default
    set foldnestmax=10      " 10 nested fold max
    nnoremap <space> za     " space open/closes folds
    set foldmethod=indent   " fold based on indent level

" 9. Movement
    " move vertically by visual line
    nnoremap j gj
    nnoremap k gk

    " move to beginning/end of line
    " Switches `^` and `0`
    nnoremap 0 ^
    nnoremap ^ 0
    "nnoremap B ^
    "nnoremap E $

    " $/^ doesn't do anything
    "nnoremap $ <nop>
    "nnoremap ^ <nop>
    " highlight last inserted text
    "nnoremap gV `[v`]

" 10. Backups
    " Put the backup files in the temporary folder
    set backup
    set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
    set backupskip=/tmp/*,/private/tmp/*
    set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
    set writebackup

" 11. Cursor Shape
    "Use a bar-shaped cursor for insert mode, even through tmux.
    if has('nvim')
        let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
        let $NVIM_TUI_ENABLE_TRUE_COLOR=1
        let $NVIM_TUI_ENABLE_SHELL_CURSOR=1
        " insert mode - line
        let &t_SI .= "\<Esc>[5 q"
        "replace mode - underline
        let &t_SR .= "\<Esc>[4 q"
        "common - block
        let &t_EI .= "\<Esc>[3 q"
    elseif empty($TMUX)
        let &t_SI = "\<Esc>]50;CursorShape=1\x7"
        let &t_EI = "\<Esc>]50;CursorShape=0\x7"
        let &t_SR = "\<Esc>]50;CursorShape=2\x7"
    else
        let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
        let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
        let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
    endif
