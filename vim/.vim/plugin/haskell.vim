" {{{1 ghcmod-vim.vim
hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = 'ghcmodType'

autocmd BufRead,BufNewFile ~/.xmonad/* call s:add_xmonad_path()
function! s:add_xmonad_path()
  if !exists('b:ghcmod_ghc_options')
    let b:ghcmod_ghc_options = []
  endif
  call add(b:ghcmod_ghc_options, '-i' . expand('~/.xmonad/lib'))
endfunction

" {{{1 neco-ghc.vim
" let &path=expand('~') . '/.local/bin/ghc-mod'
let g:necoghc_enable_detailed_browse = 1
let g:necoghc_use_stack = 1

" {{{1 haskell-vim.vim
" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
" To enable highlighting of forall
let g:haskell_enable_quantification = 1 
" To enable highlighting of mdo and rec
let g:haskell_enable_recursivedo = 1
" To enable highlighting of proc
let g:haskell_enable_arrowsyntax = 1 
" To enable highlighting of pattern
let g:haskell_enable_pattern_synonyms = 1 
" To enable highlighting of type roles
let g:haskell_enable_typeroles = 1 
" To enable highlighting of static
let g:haskell_enable_static_pointers = 1 
" To enable highlighting of backpack keywords
let g:haskell_backpack = 1
let g:haskell_classic_highlighting = 0

" Indentation
let g:haskell_indent_if = 4
let g:haskell_indent_case = 4
let g:haskell_indent_let = 4
let g:haskell_indent_guard = 4
let g:haskell_indent_where = 2
let g:haskell_indent_do = 4
let g:haskell_indent_in = 4
