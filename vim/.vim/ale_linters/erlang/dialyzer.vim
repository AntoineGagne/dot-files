let g:ale_erlang_dialyzer_executable =
            \ get(g:, 'ale_erlang_dialyzer_executable', 'dialyzer')

function! ale_linters#erlang#dialyzer#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'erlang_dialyzer_executable')
endfunction

function! ale_linters#erlang#dialyzer#GetCommand(buffer) abort
    return fnameescape(ale_linters#erlang#dialyzer#GetExecutable(a:buffer))
                \ . ' -n'
                \ . ' -Wunmatched_returns'
                \ . ' -Werror_handling'
                \ . ' -Wrace_conditions'
                \ . ' -Wunderspecs'
                \ . ' %s'
endfunction

call ale#linter#Define('erlang', {
            \ 'name': 'dialyzer',
            \ 'executable_callback': 'ale_linters#erlang#dialyzer#GetExecutable',
            \ 'command_callback': 'ale_linters#erlang#dialyzer#GetCommand',
            \ 'callback': 'ale#handlers#erlang#HandleDialyzer'
            \ })
