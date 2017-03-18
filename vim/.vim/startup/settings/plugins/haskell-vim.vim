" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
if exists("g:loaded_haskellvim_haskell")
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

    " Indentation
    let g:haskell_indent_if = 4
    let g:haskell_indent_case = 4
    let g:haskell_indent_let = 4
    let g:haskell_indent_guard = 4
    let g:haskell_indent_where = 4
    let g:haskell_indent_do = 4
    let g:haskell_indent_in = 4
    let g:haskell_classic_highlighting = 0
endif

