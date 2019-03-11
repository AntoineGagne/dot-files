let g:c_language_server = glob("/usr/bin/clangd*")
let g:cpp_language_server = glob("/usr/bin/clangd*")

let g:LanguageClient_serverCommands = {
            \ 'c': [g:c_language_server],
            \ 'cpp': [g:cpp_language_server],
            \ 'lua': ['lua-lsp'],
            \ 'python': ['pyls'],
            \ 'javascript': ['javascript-typescript-stdio'],
            \ 'haskell': ['hie', '--lsp'],
            \ 'rust': ['rls']
            \ }

" Automatically start language servers.
let g:LanguageClient_autoStart = 1
