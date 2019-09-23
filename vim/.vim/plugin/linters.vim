" {{{1 ale.vim
" The symbols displayed for errors
let g:ale_sign_error = '●'
let g:ale_sign_warning = '◆'
let g:ale_statusline_format = ['● %d', '◆ %d', '✓ ok']

" Display `ale` status messages
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'

" Write this in your vimrc file
let g:ale_set_loclist = 1
let g:ale_set_ballons = 1
let g:ale_set_quickfix = 1

let g:ale_open_list = 0
" Set this if you want to.
" This can be useful if you are combining ALE with
" some other plugin which sets quickfix errors, etc.
let g:ale_keep_list_window_open = 0

let g:ale_sign_column_always = 1

let g:ale_linters_ignore = {'haskell': ['ghc', 'cabal_ghc', 'stack_ghc']}
