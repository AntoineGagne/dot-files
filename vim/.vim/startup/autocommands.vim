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
        autocmd BufEnter *.hs set formatprg=pointfree

        " {{{2 neco-ghc
        autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
    augroup END

    augroup javascript
        autocmd!
        " autocmd FileType javascript set foldmethod=syntax
    augroup end

    augroup html
        autocmd!
        autocmd BufNewFile *.html 0r ~/.vim/templates/skeleton.html
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
