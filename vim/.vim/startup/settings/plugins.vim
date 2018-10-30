" {{{1 Autocompleters

" source ~/.vim/startup/settings/plugins/YouCompleteMe.vim
source ~/.vim/startup/settings/plugins/deoplete.vim
source ~/.vim/startup/settings/plugins/LanguageClient-neovim.vim


" {{{1 Languages

" {{{2 Rust

source ~/.vim/startup/settings/plugins/racer.vim


" {{{2 Haskell

source ~/.vim/startup/settings/plugins/haskell-vim.vim

if !has('nvim')
    source ~/.vim/startup/settings/plugins/neco-ghc.vim
    source ~/.vim/startup/settings/plugins/ghcmod-vim.vim
endif

" {{{2 Elm

source ~/.vim/startup/settings/plugins/elm-vim.vim

" {{{2 OCaml

source ~/.vim/startup/settings/plugins/ocaml-merlin.vim

" {{{2 Erlang
source ~/.vim/startup/settings/plugins/vim-erlang-compiler.vim

" {{{2 Sessions

source ~/.vim/startup/settings/plugins/sessions-vim.vim


" {{{1 UI


" {{{2 Status Line

source ~/.vim/startup/settings/plugins/vim-airline.vim


" {{{2 Colorschemes

source ~/.vim/startup/settings/plugins/gruvbox.vim


" {{{1 Linting

" source ~/.vim/startup/settings/plugins/syntastic.vim
source ~/.vim/startup/settings/plugins/ale.vim


" {{{1 Snippets
source ~/.vim/startup/settings/plugins/ultisnips.vim


" {{{1 Formatting

source ~/.vim/startup/settings/plugins/vim-table-mode.vim

" {{{1 Note taking
source ~/.vim/startup/settings/plugins/vimwiki.vim

" {{{1 Navigation
source ~/.vim/startup/settings/plugins/fzf.vim
source ~/.vim/startup/settings/plugins/vim-gutentags.vim
