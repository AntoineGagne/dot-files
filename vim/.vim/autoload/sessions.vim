function! sessions#SaveSession(save_session, generate_filename)
    let session_path = s:GetSessionCurrentPath()
    let filename = a:generate_filename()
    if !exists('filename')
        let filename = g:session_default_name
    endif
    let filename = filename . g:session_extension
    let session_file = join([session_path, filename], '/')
    call s:CreateDirectoryIfItDoesNotExists(fnamemodify(session_file, ':h'))
    execute a:save_session fnameescape(session_file)
endfunction

function! sessions#LoadSession(load_session, filename)
    execute a:load_session fnameescape(a:filename)
endfunction

function! sessions#GetSessions()
    let session_path = s:GetSessionCurrentPath()
    let paths = split(globpath(session_path, '**'), '\n')
    return filter(paths, {idx, path -> !isdirectory(path)})
endfunction

function! s:GetSessionCurrentPath()
    let working_directory = fnamemodify(getcwd(), ":t")
    return join([g:sessions_directory, working_directory], '/')
endfunction

function! s:CreateDirectoryIfItDoesNotExists(directory_name)
    if !isdirectory(a:directory_name)
        call mkdir(a:directory_name, "p", 0700)
    endif
endfunction
