" quit if syntax is already defined
if exists("b:current_syntax")
    finish
endif

" keywords
syntax keyword PerfectKeywords abstract absurd after any
syntax keyword PerfectKeywords anything as assert associative
syntax keyword PerfectKeywords axiom bag begin bool build byte
syntax keyword PerfectKeywords catch change char class commutative 
syntax keyword PerfectKeywords confined const decrease deferred
syntax keyword PerfectKeywords define done early end enum
syntax keyword PerfectKeywords exempt exists false fi final float
syntax keyword PerfectKeywords for forall from function ghost
syntax keyword PerfectKeywords goto has heap highest if idempotent
syntax keyword PerfectKeywords identity implements import in
syntax keyword PerfectKeywords inherits int interface internal
syntax keyword PerfectKeywords invariant is it keep let like
syntax keyword PerfectKeywords limited loop lowest map nonmember
syntax keyword PerfectKeywords null of on opaque operator out over
syntax keyword PerfectKeywords pair par pass post pragma pre proof
syntax keyword PerfectKeywords property rank real redefine ref
syntax keyword PerfectKeywords repeated require result satisfy
syntax keyword PerfectKeywords schema selector self seq set
syntax keyword PerfectKeywords storable super supports tag that
syntax keyword PerfectKeywords then those throw total trace triple
syntax keyword PerfectKeywords true try until value var via void
syntax keyword PerfectKeywords when within yield

" identifiers
syntax match PerfectIdentifiers /\<\(\w\|_\)\+\(\d\|\w\|_\)*\>/

" types
syntax keyword PerfectTypes map set int
syntax keyword PerfectTypes char bool void
syntax keyword PerfectTypes real byte rank
syntax keyword PerfectTypes bag seq string

" comments
syntax keyword PerfectTodo contained TODO FIXME NOTE
syntax match PerfectComments "//.*$" contains=PerfectTodo

" strings
syntax region PerfectStrings start=/"/ skip=/\\"/ end=/"/

" characters
syntax match PerfectCharacter "`\(\\\([abfnrtv\\`\"]\|(\(1[0-2][0-7]\|0\d\d\))\)\|.\)`"

" operators
syntax match PerfectOperators "<="
syntax match PerfectOperators ">="
syntax match PerfectOperators "<<"
syntax match PerfectOperators ">>"
syntax match PerfectOperators "<<="
syntax match PerfectOperators ">>="
syntax match PerfectOperators "<=="
syntax match PerfectOperators "==>"
syntax match PerfectOperators "<==>"
syntax match PerfectOperators "||"
syntax match PerfectOperators "\~\~"
syntax match PerfectOperators "\^="
syntax match PerfectOperators ":-"
syntax match PerfectOperators "::"
syntax match PerfectOperators "->"
syntax match PerfectOperators "<-"
syntax match PerfectOperators "<\->"
syntax match PerfectOperators "++"
syntax match PerfectOperators "--"
syntax match PerfectOperators "\*\*"
syntax match PerfectOperators "##"
syntax match PerfectOperators "\.\."
syntax match PerfectOperators "\.\.\."

" integers
syntax match PerfectNumbers '\<-\?\d\(_\?\d\)*\>'
syntax match PerfectNumbers '\<0[Xx][0-9A-Fa-f]\(_\?[0-9A-Fa-f]\)*\>'
syntax match PerfectNumbers '\<0[Bb][01]\(_\?[01]\)*\>'

" reals
syntax match PerfectReal '\<-\?\d\(_\?\d\)*\([Ee]-\?\d\(_\?\d\)*\)\>'
syntax match PerfectReal '\<-\?\d\(_\?\d\)*\(\.\d\(_\?\d\)*\([Ee]-\?\d\(_\?\d\)*\)\?\)\>'

highlight def link PerfectComments Comment
highlight def link PerfectKeywords Keyword
highlight def link PerfectTypes Type
highlight def link PerfectIdentifiers Identifier
highlight def link PerfectOperators Operator
highlight def link PerfectNumbers Number
highlight def link PerfectStrings String
highlight def link PerfectCharacter Character
highlight def link PerfectReal Float
