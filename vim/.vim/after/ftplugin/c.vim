let g:c_include_fixer = glob('/usr/bin/clang-include-fixer-*')

function! FixIncludes()
    if executable(g:c_include_fixer)
        execute 'pyf ' . g:c_include_fixer
    endif
endfun

setlocal foldmethod=syntax

nnoremap <silent> K :call LanguageClient#textDocument_hover()<cr>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<cr>
nnoremap <silent> gD :call LanguageClient_textDocument_implementation()<cr>
vnoremap <buffer> <silent> <leader>li :call LanguageClient_textDocument_rangeFormatting()<cr>
nnoremap <buffer> <silent> <leader>lr :call LanguageClient#textDocument_rename()<cr>
nnoremap <buffer> <silent> <leader>lf :call LanguageClient_textDocument_documentSymbol()<cr>
