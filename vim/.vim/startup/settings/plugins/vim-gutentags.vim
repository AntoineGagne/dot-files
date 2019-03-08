call directories#CreateDirectoryIfItDoesNotExists(expand('~/.vim/.tags'))
let g:gutentags_cache_dir=expand('~/.vim/.tags')
" let g:gutentags_define_advanced_commands=1

let g:gutentags_exclude_filetypes = ['c', 'cpp']
