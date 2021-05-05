let g:cpp_include_fixer = glob('/usr/bin/clang-include-fixer-*')

function! FixIncludes()
    if executable(g:cpp_include_fixer)
        execute 'pyf ' . g:cpp_include_fixer
    endif
endfun

setlocal foldmethod=syntax

if (executable('cppman'))
    autocmd FileType cpp setlocal keywordprg=cppman
endif
