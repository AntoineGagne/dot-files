" {{{1 vim-table-mode.vim
let g:table_mode_corner_corner="+"
let g:table_mode_header_fillchar="="

" {{{1 ale
let g:ale_fixers = {
            \ 'javascript': ['prettier'],
            \ 'javascriptreact': ['prettier'],
            \ 'css': ['prettier'],
            \ 'typescript': ['prettier'],
            \ 'typescriptreact': ['prettier']
            \}
let g:ale_fix_on_save = 1
