function! ale#handlers#erlang#HandleDialyzer(buffer, lines) abort
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
    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:code = l:match[2]

        call add(l:output, {
                    \ 'lnum': l:match[1] + 0,
                    \ 'lcol': 0,
                    \ 'text': l:code,
                    \ 'type': 'W'
                    \ })
    endfor

    return l:output
endfunction
