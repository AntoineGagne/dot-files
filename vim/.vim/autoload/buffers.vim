function! buffers#GetListedBuffers()
    let l:all_buffers = range(1, bufnr('$'))
    let l:buffers = filter(l:all_buffers, 'buflisted(v:val)')
    return map(l:buffers, 'bufname(v:val)')
endfunction
