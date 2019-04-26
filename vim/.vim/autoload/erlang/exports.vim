function! erlang#exports#FormatExports()
    let current_line = getline('.')
    let number_of_commas = count(l:current_line, ',')

    let i = 0
    while l:i < l:number_of_commas
        execute 'normal! ^f,a'
        let i += 1
    endwhile
endfunction
