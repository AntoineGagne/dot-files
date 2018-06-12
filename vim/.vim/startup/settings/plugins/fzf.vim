command! Buffers call fzf#run(fzf#wrap(
            \ {'source': buffers#GetListedBuffers()}))

command! -bang Buffers call fzf#run(fzf#wrap(
            \ {'source': buffers#GetListedBuffers()}, <bang>0))

command! -bang Buffers call fzf#run(fzf#wrap('buffers',
            \ {'source': buffers#GetListedBuffers()}, <bang>0))

command! Bdelete call fzf#run(fzf#wrap(
            \ {'source': buffers#GetListedBuffers(),
            \  'sink': 'bdelete'}, <bang>0))

command! -bang Bdelete call fzf#run(fzf#wrap(
            \ {'source': buffers#GetListedBuffers(),
            \  'sink': 'bdelete'}, <bang>0))

command! -bang Bdelete call fzf#run(fzf#wrap('buffers',
            \ {'source': buffers#GetListedBuffers(),
            \  'sink': 'bdelete'}, <bang>0))
