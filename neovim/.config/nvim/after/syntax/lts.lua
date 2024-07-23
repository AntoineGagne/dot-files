vim.cmd([[
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
setlocal conceallevel=1
setlocal concealcursor=nvic
]])
