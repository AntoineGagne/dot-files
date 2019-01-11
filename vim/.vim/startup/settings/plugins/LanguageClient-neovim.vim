let g:LanguageClient_serverCommands = {
            \ 'lua': ['lua-lsp'],
            \ 'python': ['pyls'],
            \ 'javascript': ['javascript-typescript-stdio'],
            \ 'haskell': ['hie', '--lsp'],
            \ 'rust': ['rls']
            \ }

" Automatically start language servers.
let g:LanguageClient_autoStart = 1
