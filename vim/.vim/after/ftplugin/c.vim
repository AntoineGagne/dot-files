let g:c_include_fixer = glob('/usr/bin/clang-include-fixer-*')

function! FixIncludes()
    if executable(g:c_include_fixer)
        execute 'pyf ' . g:c_include_fixer
    endif
endfun

setlocal foldmethod=syntax
