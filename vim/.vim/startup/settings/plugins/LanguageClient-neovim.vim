let g:LanguageClient_serverCommands = {
            \ 'c': ['clangd-7'],
            \ 'cpp': ['clangd-7'],
            \ 'lua': ['lua-lsp'],
            \ 'python': ['pyls'],
            \ 'javascript': ['javascript-typescript-stdio'],
            \ 'haskell': ['hie', '--lsp'],
            \ 'rust': ['rls']
            \ }

" Automatically start language servers.
let g:LanguageClient_autoStart = 1
