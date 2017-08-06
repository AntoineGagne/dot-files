" {{{1 General

" Case insensitive search
set ignorecase
" Case sensitive search when using capitals
set smartcase
" Search as characters are entered
set incsearch
" Highlight matches
set hlsearch

if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

" {{{1 Replace
" {{{1 Substitutions
if has('nvim')
    " Shows the effects of a command incrementally, as you type
    set inccommand=nosplit
endif
