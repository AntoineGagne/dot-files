" {{{1 Plugins

if isdirectory('_build')
    let s:option = ''
    for s:include_directory in globpath('_build', '**/include', 0, 1)
        let s:option = s:option . ' -I ' . s:include_directory
    endfor

    for s:beam_directory in globpath('_build', '**/ebin', 0, 1)
        let s:option = s:option . ' -pa ' . s:beam_directory
    endfor

    let g:ale_erlang_erlc_options = s:option
endif

let g:erlangWranglerPath = expand('~/.local/lib/wrangler/erlang/lib/wrangler-1.2.0/')
let g:erlang_completion_preview_help = 0
let g:erlang_show_errors = 0
