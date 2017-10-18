if filereadable('/usr/share/clang/clang-format-3.8/clang-format.py')
    map <C-K> :pyf /usr/share/clang/clang-format-3.8/clang-format.py<cr>
    imap <C-K> <c-o>:pyf /usr/share/clang/clang-format-3.8/clang-format.py<cr>
elseif filereadable('/usr/share/clang/clang-format.py')
    map <C-K> :pyf /usr/share/clang/clang-format.py<cr>
    imap <C-K> <c-o>:pyf /usr/share/clang/clang-format.py<cr>
endif

function! FixIncludes()
    if filereadable('/usr/share/clang/clang-include-fixer.py')
        pyf /usr/share/clang/clang-include-fixer.py
    endif
endfun

function! RenameSymbolUnderCursor()
    if filereadable('/usr/share/clang/clang-rename.py')
        pyf /usr/share/clang/clang-rename.py
    endif
endfun
