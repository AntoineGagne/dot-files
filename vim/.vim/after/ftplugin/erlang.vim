" {{{1 Completion
" Remove the preview window when using C-x C-o
set completeopt-=preview
let g:erlang_highlight_special_atoms = 1

command! ErlangFormatExports call erlang#exports#FormatExports()
