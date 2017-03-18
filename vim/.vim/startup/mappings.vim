" {{{1 Plugins

" {{{2 YouCompleteMe
nnoremap <F11> :YcmForceCompileAndDiagnostics <CR>

" {{{2 ghc-mod
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

" {{{2 Tabular
vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>
vmap a, :Tabularize /<-<CR>
vmap al :Tabularize /[\[\\|,]<CR>

"
" {{{1 Folding
nnoremap <space> za     " space open/closes folds

" {{{1 Movements
" Move vertically by visual line
nnoremap j gj
nnoremap k gk

" Move to beginning/end of line
" Switches `^` and `0`
nnoremap 0 g^
nnoremap ^ g0
nnoremap $ g$


" {{{1 Special Characters
" Inserts a « followed by a space
map <Leader>og i« <ESC> 
" Inserts a » preceded by a space
map <Leader>fg a »<ESC>
