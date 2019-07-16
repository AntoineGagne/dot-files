" {{{1 Plugins

if isdirectory('_build')
    let s:include_option = ''
    for s:include_directory in globpath('_build', '**/include', 0, 1)
        let s:include_option = s:include_option . ' -I ' . s:include_directory
    endfor

    let s:beam_include_option = ''
    for s:beam_directory in globpath('_build', '**/ebin', 0, 1)
        let s:beam_include_option = s:beam_include_option . ' -pa ' . s:beam_directory
    endfor

    let g:ale_erlang_erlc_options = s:include_option . ' ' . s:beam_include_option
endif

let g:erlangWranglerPath = expand('~/.local/lib/wrangler/erlang/lib/wrangler-1.2.0/')
let g:erlang_completion_preview_help = 0
let g:erlang_show_errors = 0
