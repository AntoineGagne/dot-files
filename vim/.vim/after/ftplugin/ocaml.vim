if isdirectory(expand('~/.opam/4.06.0/share/ocp-indent/vim'))
    set rtp+=~/.opam/4.06.0/share/ocp-indent/vim
endif
let g:deoplete#omni#input_patterns.ocaml = '[^. *\t]\.\w*|\s\w*|#'

setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
