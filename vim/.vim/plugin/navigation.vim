" {{{1 fzf.vim
let g:fzf_layout = { 'window': 'call windows#FloatingWindow()' }

command! -bang Bdelete call fzf#run(fzf#wrap(
            \ {'source': buffers#GetListedBuffers(),
            \  'sink': 'bdelete'}, <bang>0))

command! -bang Bdelete call fzf#run(fzf#wrap('buffers',
            \ {'source': buffers#GetListedBuffers(),
            \  'sink': 'bdelete'}, <bang>0))

command! LoadSession call fzf#run(fzf#wrap(
            \ {'source': sessions#GetSessions(),
            \  'sink': 'source'}, <bang>0))

command! -bang LoadSession call fzf#run(fzf#wrap('buffers',
            \ {'source': sessions#GetSessions(),
            \  'sink': 'source'}, <bang>0))

if executable('rg')
    command! -bang -nargs=* Grep call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --smart-case --hidden --follow ' . shellescape(<q-args>), 1, <bang>0)
endif

nnoremap <leader>bls :Buffers<return>
nnoremap <leader>bd :Bdelete<return>
nnoremap <leader>mls :Marks<return>
nnoremap <leader>wls :Windows<return>
nnoremap <leader>/ :BLines<return>
nnoremap <leader>ls :GitFiles<return>
