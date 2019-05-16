function! ale_linters#erlang#dialyzer#FindPlt() abort
    let plt_file = split(globpath('_build', '**/*_plt'), '\n')
    if !empty(l:plt_file)
        return l:plt_file[0]
    endif

    if !empty($REBAR_PLT_DIR)
        return expand("$REBAR_PLT_DIR/dialyzer/plt")
    endif

    return expand("$HOME/.dialyzer_plt")
endfunction

function! ale_linters#erlang#dialyzer#GetPlt(buffer) abort
    return ale#Var(a:buffer, 'erlang_plt_file')
endfunction

function! ale_linters#erlang#dialyzer#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'erlang_dialyzer_executable')
endfunction

function! ale_linters#erlang#dialyzer#GetCommand(buffer) abort
    let command = fnameescape(ale_linters#erlang#dialyzer#GetExecutable(a:buffer))
                \ . ' -n'
                \ . ' --plt ' . fnameescape(ale_linters#erlang#dialyzer#GetPlt(a:buffer))
                \ . ' -Wunmatched_returns'
                \ . ' -Werror_handling'
                \ . ' -Wrace_conditions'
                \ . ' -Wunderspecs'
                \ . ' %s'
    return l:command
endfunction

function! ale_linters#erlang#dialyzer#Handle(buffer, lines) abort
    " The output is:
    " erl_tidy_prv_fmt.erl:3: Callback info about the provider behaviour is not available
    " Unknown functions:
    "   providers:create/1
    "   rebar_app_info:dir/1
    "   rebar_app_info:name/1
    "   rebar_log:log/3
    "   rebar_state:add_provider/2
    "   rebar_state:command_parsed_args/1
    "   rebar_state:get/3
    "   rebar_state:project_apps/1
    "   rebar_utils:tup_umerge/2
    " Unknown types:
    "   rebar_state:state/0
    let l:pattern = '^\S\+:\(\d\+\): \(.\+\)$'

    let l:output = []
    for l:line in a:lines
        let match = matchlist(l:line, l:pattern)
        if len(l:match) != 0
            let l:code = l:match[2]

            call add(l:output, {
                        \ 'lnum': str2nr(l:match[1]),
                        \ 'lcol': 0,
                        \ 'text': l:code,
                        \ 'type': 'W'
                        \ })
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('erlang', {
            \ 'name': 'dialyzer',
            \ 'executable': function('ale_linters#erlang#dialyzer#GetExecutable'),
            \ 'command': function('ale_linters#erlang#dialyzer#GetCommand'),
            \ 'callback': function('ale_linters#erlang#dialyzer#Handle')
            \ })

let g:ale_erlang_dialyzer_executable =
            \ get(g:, 'ale_erlang_dialyzer_executable', 'dialyzer')
let g:ale_erlang_plt_file =
            \ get(g:, 'ale_erlang_plt_file', ale_linters#erlang#dialyzer#FindPlt())
