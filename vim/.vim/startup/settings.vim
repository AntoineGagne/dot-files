" {{{1 Plugins

" {{{2 Autocompleters

" {{{3 YouCompleteMe
let g:ycm_auto_trigger = 1

" Default 1
let g:ycm_register_as_syntastic_checker = 1
" Default 1
let g:Show_diagnostics_ui = 1

" Enable neco-ghc completion from YouCompleteMe
let g:ycm_semantic_triggers = {
            \ 'haskell' : ['.'],
            \ 'elm' : ['.']
            \}

" Will put icons in Vim's gutter on lines that have a diagnostic set.
" Turning this off will also turn off the YcmErrorLine and YcmWarningLine
" highlighting
let g:ycm_enable_diagnostic_signs = 1
let g:ycm_enable_diagnostic_highlighting = 0
" Default 0
let g:ycm_always_populate_location_list = 1
" Default 1
let g:ycm_open_loclist_on_ycm_diags = 1

" Default 1
let g:ycm_complete_in_strings = 1
" Default 0
let g:ycm_collect_identifiers_from_tags_files = 0
" Default ''
let g:ycm_path_to_python_interpreter = '' 

" Default 0 (logging to console)
let g:ycm_server_use_vim_stdout = 0
" Default info
let g:ycm_server_log_level = 'info'

" Where to search for .ycm_extra_conf.py if not found
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 1

" [ 'same-buffer', 'horizontal-split', 'vertical-split', 'new-tab' ]
let g:ycm_goto_buffer_command = 'same-buffer' 
let g:ycm_filetype_whitelist = { '*': 1 }
let g:ycm_filetype_blacklist = {
            \ 'tex' : 1
            \}
let g:ycm_key_invoke_completion = '<C-Space>'

" Set the omnicompleter
set omnifunc=syntaxcomplete#Complete


" {{{2 Languages

" {{{3 Rust

" {{{4 Racer
set hidden
let g:racer_cmd = "~/.vim/bundle/racer/target/release/racer"
let $RUST_SRC_PATH="~/rustc-1.5.0/src/"

" {{{3 Haskell

" {{{4 neco-ghc
set path="~/.local/bin/ghc-mod"
let g:necoghc_enable_detailed_browse = 1

" {{{4 haskell-vim
" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
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
    let g:haskell_classic_highlighting = 0
endif

" {{{4 ghcmod-vim
hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = 'ghcmodType'

" {{{4 Tabular
let g:haskell_tabular = 1

" {{{3 Elm
let g:elm_jump_to_error = 0
let g:elm_make_output_file = "elm.js"
let g:elm_make_show_warnings = 0
let g:elm_syntastic_show_warnings = 0
let g:elm_browser_command = ""
let g:elm_detailed_complete = 0
let g:elm_format_autosave = 0
let g:elm_setup_keybindings = 1
let g:elm_classic_highlighting = 0


" {{{2 UI

" {{{3 vim-airline
let g:airline_theme='distinguished'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#ycm#enabled = 1
let g:airline#extensions#ycm#error_symbol = 'E:'
let g:airline#extensions#ycm#warning_symbol = 'W:'
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#branch#enabled=1
let g:airline#extensions#hunks#enabled=0
let g:airline_detect_spell=1

" {{{3 Colorschemes

" {{{4 Gruvbox
colorscheme gruvbox
set background=dark
let g:gruvbox_italic=1
let g:gruvbox_invert_indent_guides=1


" {{{2 Linting

" {{{3 Syntastic
let g:syntastic_always_populate_loc_list = 1
" Always show the errors list
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0


" {{{2 Formatting

" {{{3 Markdown Table
let g:table_mode_corner_corner="+"
let g:table_mode_header_fillchar="="


" {{{1 Standard Settings

" {{{2 Files
 " Set the encoding to utf-8
scriptencoding utf-8
" Set the encoding to utf-8
set encoding=utf-8


" {{{2 Editor
set tags+=~/.tags
set tags+=./tags
if has('nvim')
    let g:python_host_prog = expand('~') . '/.virtualenvs/neovim2/bin/python'
    let g:python3_host_prog = expand('~') . '/.virtualenvs/neovim3/bin/python'
endif


" {{{2 UI

" {{{3 General
" Enable syntax processing
syntax enable
" Load filetype-specific indent files
filetype indent on
" Set the command window height
set cmdheight=1
" Instead of failing a command because of unsaved changes, instead raise a 
" dialogue asking if you wish to save changed files
set confirm
" Highlight current line
set cursorline
" Always display the status line, even if only one window is displayed
set laststatus=2
" Redraw only when we need to   
set lazyredraw
" Show line numbers
set number
" Display the cursor position on the last line of the screen or in the status 
" line of a window
set ruler
" Show command in bottom bar
set showcmd
" Highlight matching [{()}]
set showmatch
" Use visual bell instead of beeping when doing something wrong
set visualbell
" Visual autocomplete for command menu
set wildmenu
set termguicolors
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
endif
set cole=2
let g:tex_conceal='adgms'

" {{{3 Cursor Shapes
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


" {{{2 Spaces & Tabs
" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start
set autoindent
" Show spaces visually
set list
" Display spaces as dots and tabs as arrows
set listchars=space:•,tab:⟶\ 
" Number of visual spaces per TAB
set tabstop=4
" Number of spaces in tab when editing
set softtabstop=4
" Number of spaces inserted for indentation
set shiftwidth=4
" Replace tabs by spaces
set expandtab
" Make tab insert indents instead of tabs at the beginning of a line
set smarttab


" {{{2 Searching
" Case insensitive search
set ignorecase
" Case sensitive search when using capitals
set smartcase
" Search as characters are entered
set incsearch
" Highlight matches
set hlsearch


" {{{2 Folding
" Enable folding
set foldenable
" Open most folds by default
set foldlevelstart=10
" 10 nested fold max
set foldnestmax=10
" Fold based on indent level
set foldmethod=indent


" {{{2 Backups
" Put the backup files in the temporary folder
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup


" {{{2 Languages
" Enables spell check
set spell
" Enables the english spell checker
set spelllang=en
