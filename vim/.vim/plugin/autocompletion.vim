" {{{1 deoplete.vim
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

" Let <Tab> also do completion
" inoremap <silent><expr> <Tab>
" \ pumvisible() ? "\<C-n>" :
" \ deoplete#mappings#manual_complete()

" Close the documentation window when completion is done
" autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" {{{1 LanguageClient-neovim.vim
let g:c_language_server = glob('/usr/bin/clangd*')
let g:cpp_language_server = glob('/usr/bin/clangd*')

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
