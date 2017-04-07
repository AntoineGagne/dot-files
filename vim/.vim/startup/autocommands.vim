" {{{1 Haskell
autocmd BufEnter *.hs set formatprg=pointfree

" {{{2 neco-ghc
autocmd BufWritePost *.hs GhcModCheckAndLintAsync
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" {{{1 LaTeX & TeX
au BufNewFile,BufRead,BufReadPre,BufReadPost *.sty setfiletype plaintex
au BufNewFile,BufRead,BufReadPre,BufReadPost *.cls setfiletype plaintex
au BufNewFile,BufRead,BufReadPre,BufReadPost *.tex setfiletype plaintex
au BufNewFile,BufRead,BufReadPre,BufReadPost *.tikz setfiletype plaintex
au BufNewFile,BufRead,BufReadPre,BufReadPost *.bib setfiletype plaintex
