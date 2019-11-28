syntax case match

syntax match LogsKeys '\s\l\+='
syntax match LogsKeys '^\l\+='
syntax match LogsDate '\<\d\{4}-\d\{2}-\d\{2}T\d\{2}:\d\{2}:\d\{2}.\d\++\d\{2}:\d\{2}\>'

" strings
syntax match LogsSpecialCharacter contained "\(\\\([abfnrtv\\`\"]\|(\(1[0-2][0-7]\|0\d\d\))\)\)"
syntax match LogsRegularCharacter contained "."
syntax region LogsStrings start=/"/ end=/"/ oneline contains=LogsSpecialCharacter,@LogsRegularCharacter

" characters
syntax match LogsCharacter "`\(\\\([abfnrtv\\`\"]\|(\(1[0-2][0-7]\|0\d\d\))\)\|.\)`"

highlight default link LogsKeys Keyword
highlight default link LogsDate Comment

highlight default link LogsStrings String
highlight default link LogsSpecialCharacter SpecialChar
highlight default link LogsCharacter Character
