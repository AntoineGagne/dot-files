let g:ale_erlang_dialyzer_executable =
            \ get(g:, 'ale_erlang_dialyzer_executable', 'dialyzer')

function! ale_linters#erlang#dialyzer#GetPlt(buffer) abort
    let plt_file = split(globpath('_build', '**/*_plt'), '\n')
    if !empty(l:plt_file)
        return l:plt_file[0]
    endif

    if !empty($REBAR_PLT_DIR)
        return expand("$REBAR_PLT_DIR/dialyzer/plt")
    endif

    return expand("$HOME/.dialyzer_plt")
endfunction

function! ale_linters#erlang#dialyzer#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'erlang_dialyzer_executable')
endfunction

function! ale_linters#erlang#dialyzer#GetCommand(buffer) abort
    let plt_file = ale_linters#erlang#dialyzer#GetPlt(a:buffer)
    return fnameescape(ale_linters#erlang#dialyzer#GetExecutable(a:buffer))
                \ . ' -n'
                \ . ' --plt ' . l:plt_file
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
