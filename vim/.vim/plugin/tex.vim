setlocal foldmethod=syntax

augroup tex
    autocmd!

    autocmd BufNewFile *.tex 0r ~/.vim/templates/skeleton.tex
augroup END
