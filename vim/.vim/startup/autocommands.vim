augroup template
    autocmd!
    autocmd BufNewFile * silent! 0r $HOME/.vim/templates/skeleton.%:e
augroup END
