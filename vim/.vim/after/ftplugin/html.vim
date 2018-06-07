augroup html
    autocmd!
    autocmd BufNewFile *.html 0r ~/.vim/templates/skeleton.html
augroup END

if executable("webman")
    setlocal keywordprg=webman\ -t\ html\ -s
endif
