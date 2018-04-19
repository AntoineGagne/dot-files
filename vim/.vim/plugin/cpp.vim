setlocal foldmethod=syntax

if (executable("cppman"))
    autocmd FileType cpp set keywordprg=cppman
endif
