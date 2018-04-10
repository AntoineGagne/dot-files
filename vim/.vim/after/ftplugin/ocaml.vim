if isdirectory(expand('~/.opam/4.06.0/share/ocp-indent/vim'))
    set rtp+=~/.opam/4.06.0/share/ocp-indent/vim
endif
let g:deoplete#omni#input_patterns.ocaml = '[^. *\t]\.\w*|\s\w*|#'

set tabstop=2
set softtabstop=2
set shiftwidth=2
