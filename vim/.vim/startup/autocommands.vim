if has("autocmd")
    " {{{1 Haskell
    augroup haskell
        autocmd!
        autocmd BufEnter *.hs set formatprg=pointfree

        " {{{2 neco-ghc
        " autocmd BufWritePost *.hs GhcModCheckAndLintAsync
        autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
        " autocmd FileType haskell set foldmethod=indent
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
        autocmd BufNewFile,BufRead,BufReadPre,BufReadPost *.sty setfiletype plaintex
        autocmd BufNewFile,BufRead,BufReadPre,BufReadPost *.cls setfiletype plaintex
        autocmd BufNewFile,BufRead,BufReadPre,BufReadPost *.tex setfiletype plaintex
        autocmd BufNewFile,BufRead,BufReadPre,BufReadPost *.tikz setfiletype plaintex
        autocmd BufNewFile,BufRead,BufReadPre,BufReadPost *.bib setfiletype plaintex
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
        " autocmd FileType python set foldmethod=indent
    augroup END

    augroup cpp
        autocmd!
        autocmd FileType cpp set foldmethod=syntax
    augroup END

    augroup clang
        autocmd!
        autocmd FileType c set foldmethod=syntax
    augroup END
endif
