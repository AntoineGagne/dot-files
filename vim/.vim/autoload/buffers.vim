function! buffers#GetListedBuffers()
    let a:all_buffers = range(1, bufnr('$'))
    let a:buffers = filter(a:all_buffers, 'buflisted(v:val)')
    return map(a:buffers, 'bufname(v:val)')
endfunction
