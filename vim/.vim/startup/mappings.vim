" {{{1 Plugins

" {{{2 YouCompleteMe
nnoremap <F11> :YcmForceCompileAndDiagnostics <CR>

" {{{1 Folding
" space open/closes folds
nnoremap <space> za

" {{{1 Movements
" {{{2 File
" Move vertically by visual line
nnoremap j gj
nnoremap k gk

" Move to beginning/end of line
" Switches `^` and `0`
nnoremap 0 g^
nnoremap ^ g0
nnoremap $ g$

" {{{2 Buffers
" Move to nth next buffer
nnoremap <leader>bn :bnext<CR>
" Move to nth previous buffer
nnoremap <leader>bp :bprevious<CR>


" {{{1 Special Characters
" Inserts a « followed by a space
map <Leader>og i« <ESC> 
" Inserts a » preceded by a space
map <Leader>fg a »<ESC>
