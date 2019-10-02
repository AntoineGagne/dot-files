function! erlang#man#ViewHelp(name)
    execute 'normal :new | r !erl -man ' . a:name . ''
    execute 'normal :setlocal filetype=man '
    execute 'normal :setlocal buftype=help '
    execute 'normal :setlocal bufhidden=hide '
    execute 'normal :setlocal noswapfile '
    execute 'normal :setlocal nobuflisted '
endfunction
