function! directories#CreateDirectories(...)
    for l:directory in a:000
        call directories#CreateDirectoryIfItDoesNotExists(l:directory)
    endfor
endfunction

function! directories#CreateDirectoryIfItDoesNotExists(directory_name)
    if !isdirectory(a:directory_name)
        call mkdir(a:directory_name, '', 0700)
    endif
endfunction
