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
set conceallevel=2
let g:tex_conceal='adgms'


" {{{1 Top Panels

" Set the terminal's title
set title


" {{{1 Side Panels

" Show line numbers
set number

" {{{1 Separators
set fillchars=vert:│,fold:─


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
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
  \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
  \,sm:block-blinkwait175-blinkoff150-blinkon175
" Set the number of screen lines above and below the cursor
set scrolloff=4


" {{{1 Command Window

" Set the command window height
set cmdheight=1
" Show command in bottom bar
set showcmd
" Visual autocomplete for command menu
set wildmenu

" {{{1 Plugins

" {{{2 vim-airline.vim
let g:airline_theme='distinguished'
let g:airline_powerline_fonts = 1

let g:airline#extensions#ale#enabled = 1
let airline#extensions#ale#error_symbol = 'E:'
let airline#extensions#ale#warning_symbol = 'W:'

let g:airline#extensions#gutentags#enabled = 1
let g:airline#extensions#tabline#enabled = 1

let g:airline_detect_spell=1

" if !exists('g:airline_symbols')
"     let g:airline_symbols = {}
" endif

" Unicode Symbols
" let g:airline_left_sep = '▶'
" let g:airline_left_alt_sep = '⟩'
" let g:airline_right_sep = '◀'
" let g:airline_right_alt_sep = '⟨'
" let g:airline_symbols.linenr = '␊'
" let g:airline_symbols.branch = '⎇'
" let g:airline_symbols.paste = 'ρ'
" let g:airline_symbols.paste = 'Þ'
" let g:airline_symbols.paste = '∥'
" let g:airline_symbols.whitespace = 'Ξ'
" let g:airline_symbols.readonly = '🔒'

" {{{2 chromatica.vim
let g:chromatica#libclang_path='/usr/lib/x86_64-linux-gnu/libclang-7.so.1'
let g:chromatica#enable_at_startup = 1
let g:chromatica#responsive_mode = 1

" {{{2 gruvbox.vim
set background=dark
colorscheme gruvbox
" let g:gruvbox_italic = 1
" let g:gruvbox_improved_strings = 1
" let g:gruvbox_improved_warnings = 1
" let g:gruvbox_invert_indent_guides = 1
" let g:gruvbox_italicize_strings = 1

if !empty($TMUX)
    set t_ut=
endif
