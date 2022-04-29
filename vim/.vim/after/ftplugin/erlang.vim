" {{{1 Completion
" Remove the preview window when using C-x C-o
set completeopt-=preview
let g:erlang_highlight_special_atoms = 1

command! ErlangFormatExports call erlang#exports#FormatExports()
command! -nargs=1 ErlangMan call erlang#man#ViewHelp(<f-args>)
setlocal keywordprg=:ErlangMan

" nnoremap <silent> gd <cmd>lua vim.lsp.buf.declaration()<CR>
" nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
" nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
" nnoremap <silent> gD <cmd>lua vim.lsp.buf.implementation()<CR>
" nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
" nnoremap <silent> 1gD <cmd>lua vim.lsp.buf.type_definition()<CR>
" nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
" nnoremap <silent> g0 <cmd>lua vim.lsp.buf.document_symbol()<CR>
" 
" setlocal omnifunc=v:lua.vim.lsp.omnifunc
