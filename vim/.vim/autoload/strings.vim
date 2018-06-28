function! strings#Chomp(string)
    return substitute(a:string, '[[:cntrl:]]', '', 'g')
endfunction
