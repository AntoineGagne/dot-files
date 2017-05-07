" {{{1 Visual Spaces & Tabs

" Show spaces visually
set list
" Display spaces as dots and tabs as arrows
" set listchars=space:•,tab:⟶\ ,nbsp:␣
set listchars=space:•,tab:→\ ,nbsp:␣


" {{{1 Backspace Behavior

" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start


" {{{1 Spaces & Tabs Behavior
set autoindent
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
