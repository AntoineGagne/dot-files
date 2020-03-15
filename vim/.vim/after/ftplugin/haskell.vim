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
if executable("pointfree")
    setlocal formatprg=pointfree
endif

nnoremap <silent> gd <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0 <cmd>lua vim.lsp.buf.document_symbol()<CR>

setlocal omnifunc=v:lua.vim.lsp.omnifunc
