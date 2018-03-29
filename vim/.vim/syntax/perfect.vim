" quit if syntax is already defined
if exists("b:current_syntax")
    finish
endif

" keywords
syntax keyword PerfectKeywords class const final
syntax keyword PerfectKeywords abstract tag of
syntax keyword PerfectKeywords interface in var schema
syntax keyword PerfectKeywords post build function end
syntax keyword PerfectKeywords invariant forall assert
syntax keyword PerfectKeywords pre property

" types
syntax keyword PerfectTypes map set int

" comments
syntax keyword PerfectTodo contained TODO FIXME NOTE
syntax match PerfectComments "//.*" contains=PerfectTodo

" operators
syntax match PerfectOperators "->"
syntax match PerfectOperators "="
syntax match PerfectOperators "!="
syntax match PerfectOperators "^="
syntax match PerfectOperators "\.\."
syntax match PerfectOperators "::"
syntax match PerfectOperators ":-"
syntax match PerfectOperators ">"
syntax match PerfectOperators "<<="
syntax match PerfectOperators "<<"
syntax match PerfectOperators ">="
syntax match PerfectOperators "<="
syntax match PerfectOperators "\~"
syntax match PerfectOperators "##"
syntax match PerfectOperators "+"
syntax match PerfectOperators "++"
syntax match PerfectOperators "!"
syntax match PerfectOperators "==>"
syntax match PerfectOperators "[(){}\[\]]"

" types
syntax match PerfectNumbers '\d\+'
syntax match PerfectNumbers '[-+]\d\+'

highlight def link PerfectComments Comment
highlight def link PerfectKeywords Keyword
highlight def link PerfectTypes Type
highlight def link PerfectOperators Operator
highlight def link PerfectNumbers Number
