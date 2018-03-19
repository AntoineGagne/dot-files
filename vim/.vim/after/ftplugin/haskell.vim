if !has('nvim')
    map <silent> tw :GhcModTypeInsert<CR>
    map <silent> ts :GhcModSplitFunCase<CR>
    map <silent> tq :GhcModType<CR>
    map <silent> te :GhcModTypeClear<CR>

    " let g:haskellmode_completion_ghc = 0
    " autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
endif

" {{{1 Syntax Highlighting
" Fix Gruvbox colorscheme issues
hi! link haskellType GruvboxYellow
hi! link haskellIdentifier GruvboxGreen
hi! link haskellOperators GruvboxBlue
hi! link haskellBacktick haskellOperators
hi! link haskellDecl GruvboxAqua
" hi! link haskellConditional GruvboxRed
" hi! link haskellKeyword GruvboxRed
" hi! link haskellLet GruvboxRed
" hi! link haskellWhere GruvboxRed
