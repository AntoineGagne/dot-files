if has("autocmd")
    augroup markdown
        autocmd!
        autocmd! BufEnter *.md set filetype=pandoc
    augroup END

    augroup graphviz
        autocmd!
        autocmd! BufEnter *.gv set filetype=dot
    augroup END

    " {{{1 Haskell
    augroup haskell
        autocmd!
        if executable("pointfree")
            autocmd BufEnter *.hs set formatprg=pointfree
        endif

        if executable("haskellman")
            autocmd FileType haskell set keywordprg=haskellman
        endif

        " {{{2 neco-ghc
        autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
    augroup END

    augroup javascript
        autocmd!
        " autocmd FileType javascript set foldmethod=syntax

        if executable("webman")
            autocmd FileType javascript  set keywordprg=webman\ -t\ js\ -s
        endif
    augroup end

    augroup html
        autocmd!
        autocmd BufNewFile *.html 0r ~/.vim/templates/skeleton.html

        if executable("webman")
            autocmd FileType html set keywordprg=webman\ -t\ html\ -s
        endif
    augroup END

    augroup css
        autocmd!

        if executable("webman")
            autocmd FileType html set keywordprg=webman\ -t\ css\ -s
        endif
    augroup END

    " {{{1 LaTeX & TeX
    augroup latex
        autocmd!
        autocmd FileType plaintex set foldmethod=syntax
        autocmd FileType tex set foldmethod=syntax
        autocmd BufNewFile *.tex 0r ~/.vim/templates/skeleton.tex
    augroup END

    augroup shell
        autocmd!
        autocmd BufNewFile *.sh 0r ~/.vim/templates/skeleton.sh
    augroup END

    augroup bash
        autocmd!
        autocmd BufNewFile *.bash 0r ~/.vim/templates/skeleton.bash
    augroup END

    augroup python
        autocmd!
    augroup END

    augroup cpp
        autocmd!
        autocmd FileType cpp set foldmethod=syntax

        if (executable("cppman"))
            autocmd FileType cpp set keywordprg=cppman
        endif
    augroup END

    augroup clang
        autocmd!
        autocmd FileType c set foldmethod=syntax
    augroup END

    augroup octave
        autocmd!
        autocmd BufRead,BufNewFile *.m,*.oct set filetype=octave
    augroup END

    augroup systemd
        autocmd!
        autocmd! BufEnter *.service set filetype=systemd
    augroup END
endif
