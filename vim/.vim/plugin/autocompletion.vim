" {{{1 deoplete.vim
let g:deoplete#enable_at_startup = 1
" let g:deoplete#enable_smart_case = 1
set completeopt=menu,noselect

" Let <Tab> also do completion
" inoremap <silent><expr> <Tab>
" \ pumvisible() ? "\<C-n>" :
" \ deoplete#mappings#manual_complete()

" Close the documentation window when completion is done
" autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" {{{1 ALE
let g:ale_completion_enabled = 0

" {{{1 LanguageClient-neovim.vim
" let g:c_language_server = glob('/usr/bin/clangd*')
" let g:cpp_language_server = glob('/usr/bin/clangd*')

" let g:LanguageClient_loggingFile = '/tmp/LanguageClient.log'
" let g:LanguageClient_serverCommands = {
"             \ 'bib': ['texlab'],
"             \ 'c': [g:c_language_server],
"             \ 'cpp': [g:cpp_language_server],
"             \ 'elixir': ['elixir-ls'],
"             \ 'fsharp': g:fsharp#languageserver_command,
"             \ 'latex': ['texlab'],
"             \ 'lua': ['lua-lsp'],
"             \ 'plaintex': ['texlab'],
"             \ 'python': ['pyls'],
"             \ 'javascript': ['javascript-typescript-stdio'],
"             \ 'haskell': ['hie', '--lsp'],
"             \ 'rust': ['ra_lsp_server'],
"             \ 'sh': ['bash-language-server', 'start'],
"             \ 'tex': ['texlab']
"             \ }

" Automatically start language servers.
" let g:LanguageClient_autoStart = 1

" {{{ nvim-lsp
lua << EOF
  local nvim_lsp = require 'lspconfig'
  local lsp = require 'lsp'
  nvim_lsp.bashls.setup(lsp.custom_settings.bashls)
  nvim_lsp.elmls.setup(lsp.custom_settings.elmls)
  nvim_lsp.hls.setup(lsp.custom_settings.hls)
  nvim_lsp.texlab.setup(lsp.custom_settings.texlab)
  nvim_lsp.tsserver.setup(lsp.custom_settings.tsserver)
  nvim_lsp.rust_analyzer.setup(lsp.custom_settings.rust_analyzer)
EOF

" {{{1 Wildmenu
set wildmode=full
" set wildoptions=pum
