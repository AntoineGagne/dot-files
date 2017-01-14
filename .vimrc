"{{{1 1. Files
filetype off         " required for Vundle
scriptencoding utf-8 " Set the encoding to utf-8
set encoding=utf-8   " Set the encoding to utf-8

"{{{1 2. Editor
set nocompatible " no longer compatible with Vi, required for Vundle
set tags+=~/.tags
set tags+=./tags

"{{{1 3. Plugins

let g:python_host_prog = '/home/twain/.virtualenvs/neovim2/bin/python'
let g:python3_host_prog = '/home/twain/.virtualenvs/neovim3/bin/python'
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
    " alternatively, pass a path where Vundle should install plugins
    "call vundle#begin('~/some/path/here')

    " let Vundle manage Vundle, required
    Plugin 'gmarik/Vundle.vim'

    "{{{2 Autocompletion plugin
    Plugin 'Valloric/YouCompleteMe'
    "Plugin 'Shougo/neocomplete.vim'

    "{{{2 Status bar
    Plugin 'bling/vim-airline'
    Plugin 'vim-airline/vim-airline-themes'

    "{{{2 Rust autocompletion bundle
    Plugin 'phildawes/racer'

    "{{{2 Syntax highlighter for the Rust language
    Plugin 'rust-lang/rust.vim'

    "{{{2 Haskell Syntax Checking
    Plugin 'eagletmt/ghcmod-vim'

    "{{{2 Autocompletion for the Haskell language
    Plugin 'eagletmt/neco-ghc'

    "{{{2 Syntax highlighting for the Haskell language
    Plugin 'neovimhaskell/haskell-vim'

    "{{{2 Spell checker (requires further installations)
    " Plugin 'vim-scripts/LanguageTool'

    "{{{2 HTML5 Syntax Highlighting
    Plugin 'othree/html5.vim'

    "{{{2 Show filesystem as a tree
    Plugin 'scrooloose/nerdtree'

    "{{{2 Syntax checking
    Plugin 'scrooloose/syntastic'

    "{{{2 Asynchronous Execution
    Plugin 'Shougo/vimproc.vim'

    "{{{2 Elm Syntax Highlighting
    Plugin 'elmcast/elm-vim'

    "{{{2 Pandoc Syntax
    Plugin 'vim-pandoc/vim-pandoc'
    Plugin 'vim-pandoc/vim-pandoc-syntax'

    "{{{2 Markdown Tables
    Plugin 'dhruvasagar/vim-table-mode'

    "{{{2 ColorScheme 
    Plugin 'morhetz/gruvbox'

    "{{{2 Utility functions
    Plugin 'MarcWeber/vim-addon-mw-utils'
    Plugin 'tomtom/tlib_vim'

    "{{{2 Snippets
    Plugin 'garbas/vim-snipmate'

    "{{{2 Aligning
    Plugin 'godlygeek/tabular'

    "{{{2 Autocomplete
    Plugin 'ervandew/supertab'

    "{{{2 Show the CSS colors visually
    " Plugin 'skammer/vim-css-color'

    "{{{2 Haskell unicode symbols
    Plugin 'Twinside/vim-haskellConceal'

    "{{{2 LaTeX plugins
    "Plugin 'lervag/vimtex'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

"{{{1 4. Plugins Configuration

"{{{2 YouCompleteMe options
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
let g:ycm_filetype_blacklist = {
            \ 'tex' : 1
            \}
let g:ycm_key_invoke_completion = '<C-Space>'

nnoremap <F11> :YcmForceCompileAndDiagnostics <CR>

" Set the omnicompleter
set omnifunc=syntaxcomplete#Complete

" Racer required commands
set hidden
let g:racer_cmd = "~/.vim/bundle/racer/target/release/racer"
let $RUST_SRC_PATH="/home/twain/rustc-1.5.0/src/"

"{{{2 Ghc-mod
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

"{{{2 Supertab
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

if has("gui_running")
    imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
    if has("unix")
        inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
    endif
endif

"{{{2 neco-ghc options
autocmd BufWritePost *.hs GhcModCheckAndLintAsync
set path="~/.local/bin/ghc-mod"
let g:necoghc_enable_detailed_browse = 1
" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

"{{{2 Haskell vim options
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
let g:haskell_classic_highlighting = 0

"{{{2 Haskell
autocmd BufEnter *.hs set formatprg=pointfree

"{{{2 ghcmod-vim
hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = 'ghcmodType'

"{{{2 vim-airline
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

"{{{2 Syntastic
" set statusline=%f\ %h%w%m%r\ 
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" set statusline+=%=%(%l,%c%V\ %=\ %P%)
let g:syntastic_always_populate_loc_list = 1
" Always show the errors list
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"{{{2 Gruvbox Settings
"let g:gruvbox_termcolors=16

"{{{2 Elm
let g:elm_jump_to_error = 0
let g:elm_make_output_file = "elm.js"
let g:elm_make_show_warnings = 0
let g:elm_syntastic_show_warnings = 0
let g:elm_browser_command = ""
let g:elm_detailed_complete = 0
let g:elm_format_autosave = 0
let g:elm_setup_keybindings = 1
let g:elm_classic_highlighting = 0

"{{{2 Tabular
let g:haskell_tabular = 1

vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>
vmap a, :Tabularize /<-<CR>
vmap al :Tabularize /[\[\\|,]<CR>

" To ignore plugin indent changes, instead use:
"filetype plugin on

"{{{2 Vundle commands
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

"{{{2 Markdown table
let g:table_mode_corner_corner="+"
let g:table_mode_header_fillchar="="

"{{{1 5. Colors 

"{{{2 Molokai
"colorscheme molokai         " awesome colorscheme
"let g:regash256 = 1

"{{{2 Solarized
"colorscheme solarized
"set background=light
"let g:solarized_termcolors=256

"{{{2 One Dark
"syntax on
"colorscheme onedark

"{{{2 Darcula
"colorscheme darcula
    
"{{{2 Gruvbox
colorscheme gruvbox
set background=dark
let g:gruvbox_italic=1
let g:gruvbox_invert_indent_guides=1
"let g:gruvbox_improved_strings=1
"let g:gruvbox_improved_warnings=1

"{{{1 6. Space & Tabs
syntax enable                  " enable syntax processing
set backspace=indent,eol,start " Allow backspacing over autoindent, line breaks and start of insert action
set autoindent
set list                       " Show spaces visually
set listchars=space:•,tab:⟶\ 
set tabstop=4                  " number of visual spaces per TAB
set softtabstop=4              " number of spaces in tab when editing
set shiftwidth=4               " number of spaces inserted for indentation
set expandtab                  " replace tabs by spaces
set smarttab                   " make tab insert indents instead of tabs at the beginning of a line

"{{{1 7. UI Configuration
filetype indent on    " load filetype-specific indent files
set cmdheight=1       " Set the command window height
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
set termguicolors
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
endif
set cole=2
let g:tex_conceal='adgms'

"{{{1 8. Searching
set ignorecase        " Case insensitive search
set smartcase         " Case sensitive search when using capitals
set incsearch         " search as characters are entered
set hlsearch          " highlight matches

"{{{1 9. Folding
set foldenable          " enable folding
set foldlevelstart=10   " open most folds by default
set foldnestmax=10      " 10 nested fold max
nnoremap <space> za     " space open/closes folds
set foldmethod=indent   " fold based on indent level

"{{{1 10. Movement
" move vertically by visual line
nnoremap j gj
nnoremap k gk

" move to beginning/end of line
" Switches `^` and `0`
nnoremap 0 g^
nnoremap ^ g0
nnoremap $ g$
"nnoremap B ^
"nnoremap E $

" $/^ doesn't do anything
"nnoremap $ <nop>
"nnoremap ^ <nop>
" highlight last inserted text
"nnoremap gV `[v`]

"{{{1 11. Backups
" Put the backup files in the temporary folder
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup

"{{{1 12. Cursor Shape
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

"{{{1 12. Languages
set spell " Enables spell check
set spelllang=en " Enables the english spell checker

"{{{2 Special Characters
" Inserts a «
map <Leader>og i« <ESC> 
" Inserts a »
map <Leader>fg a »<ESC>

"{{{2 Programming languages
" Sets the filetype for tex files
au BufNewFile,BufRead *.sty setfiletype tex
au BufNewFile,BufRead *.cls setfiletype tex
au BufNewFile,BufRead *.tex setfiletype tex
au BufNewFile,BufRead *.tikz setfiletype tex
au BufNewFile,BufRead *.bib setfiletype tex

"{{{2 Vim language
autocmd BufNewFile,BufRead *.vim setlocal foldmethod=marker
" au BufNewFile,BufRead *.sty *.cls *.tex *.tikz :setlocal syntax=tex
