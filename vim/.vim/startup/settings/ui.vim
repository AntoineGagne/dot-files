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

" Set transparent background
highlight Normal guibg=none

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
    set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
      \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
      \,sm:block-blinkwait175-blinkoff150-blinkon175
elseif empty($TMUX)
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
    let &t_SR = "\<Esc>]50;CursorShape=2\x7"
else
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
    let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
endif
" Set the number of screen lines above and below the cursor
set scrolloff=4


" {{{1 Command Window

" Set the command window height
set cmdheight=1
" Show command in bottom bar
set showcmd
" Visual autocomplete for command menu
set wildmenu
