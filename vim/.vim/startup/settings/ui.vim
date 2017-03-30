" {{{1 General

" Load filetype-specific indent files
filetype indent on
" Redraw only when we need to
set lazyredraw
" Use visual bell instead of beeping when doing something wrong
set visualbell


" {{{1 Colors

" Enable syntax processing
syntax enable
set termguicolors
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
endif
set cole=2
let g:tex_conceal='adgms'


" {{{1 Top Panels

" Set the terminal's title
set title


" {{{1 Side Panels

" Show line numbers
set number


" {{{1 Status Line

" Display the cursor position on the last line of the screen or in the status 
" line of a window
set ruler
" Always display the status line, even if only one window is displayed
set laststatus=2

" {{{1 Cursor

" Highlight matching [{()}]
set showmatch
" Highlight current line
set cursorline
" Use a bar-shaped cursor for insert mode, even through tmux.
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


" {{{1 Command Window

" Set the command window height
set cmdheight=1
" Show command in bottom bar
set showcmd
" Visual autocomplete for command menu
set wildmenu
