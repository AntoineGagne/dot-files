" {{{1 General

" Case insensitive search
set ignorecase
" Case sensitive search when using capitals
set smartcase
" Search as characters are entered
set incsearch
" Highlight matches
set hlsearch

" {{{1 Substitutions
if has('nvim')
    " Shows the effects of a command incrementally, as you type
    set inccommand=nosplit
endif
