let g:ocaml_opamshare = substitute(system('opam config var share'),'\n$','','''')
let g:ocaml_ocp_indent = g:ocaml_opamshare . '/ocp-indent/vim'
let g:ocaml_merlin = g:ocaml_opamshare . '/merlin/vim'

if isdirectory(g:ocaml_ocp_indent)
    execute 'set runtimepath+=' . g:ocaml_ocp_indent
endif
if isdirectory(g:ocaml_merlin)
    execute 'set runtimepath+=' . g:ocaml_merlin
endif
