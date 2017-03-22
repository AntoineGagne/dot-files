let g:syntastic_always_populate_loc_list = 1
" Always show the errors list
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" Put all the checkers errors together
let g:syntastic_aggregate_errors = 1

" {{{1 Python
let g:syntastic_python_checkers = ['mypy', 'flake8', 'pylint', 'python']
