function! erlang#man#ViewHelp(name)
    execute 'normal :new | r !erl -man ' . a:name . ''
    execute 'normal :set filetype=man '
endfunction
