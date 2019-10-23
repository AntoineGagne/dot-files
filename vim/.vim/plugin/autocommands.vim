augroup template
    autocmd!
    autocmd BufNewFile * silent! 0r $HOME/.vim/templates/skeleton.%:e
augroup END

augroup ui
    autocmd!
    autocmd ColorScheme * highlight Normal ctermbg=none guibg=none
augroup END
