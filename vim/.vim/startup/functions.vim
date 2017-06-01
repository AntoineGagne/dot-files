function! CreateDirectoryIfItDoesNotExists(directory_name)
    if !isdirectory(a:directory_name)
        call mkdir(a:directory_name)
    endif
endfun
