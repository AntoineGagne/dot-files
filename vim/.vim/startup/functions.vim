function! CreateDirectoryIfItDoesNotExists(directory_name)
    if !isdirectory(a:directory_name)
        call mkdir(a:directory_name)
        call system('chmod 0700 ' . shellescape(a:directory_name))
    endif
endfun
