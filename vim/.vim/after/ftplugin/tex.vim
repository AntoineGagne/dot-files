" Enable the french spell checker
setlocal spelllang=fr

" Enable syntax folding
let g:tex_fold_enabled=1

" Open file in LaTeX mode by default
let g:tex_flavor="latex"

" a = accents/ligatures
" d = delimiters
" m = math symbols
" g = Greek
" s = superscripts/subscripts
let g:tex_conceal='adgm'

" Taken from https://vi.stackexchange.com/a/2360
" (consulted on Wed Mar  7 16:34:20 EST 2018)
exec("setlocal dictionary+=" . $HOME . "/.vim/dictionaries/" . expand('<amatch>'))
set completeopt=menuone,longest,preview
set complete+=k
