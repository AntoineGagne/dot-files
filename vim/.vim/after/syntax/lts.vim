if exists('g:no_vim_conceal') || !has('conceal') || &enc != 'utf-8'
    finish
endif

syntax match ltsNiceOperator "->" conceal cchar=→
syntax match ltsNiceOperator "\:\:" conceal cchar=∷
syntax match ltsNiceOperator "\.\." conceal cchar=‥
syntax match ltsNiceOperator "<=\ze[^<]" conceal cchar=≤
syntax match ltsNiceOperator ">=\ze[^>]" conceal cchar=≥
syntax match ltsNiceOperator "==" conceal cchar=≡
syntax match ltsNiceOperator "!=" conceal cchar=≠
syntax match ltsNiceOperator ">>" conceal cchar=»
syntax match ltsNiceOperator "<<" conceal cchar=«
syntax match ltsNiceOperator "||" conceal cchar=∥

highlight link ltsNiceOperator Operator
highlight! link Conceal Operator
setlocal conceallevel=2
