" {{{1 Files

 " Set the encoding to utf-8
scriptencoding utf-8
" Set the encoding to utf-8
set encoding=utf-8
" Instead of failing a command because of unsaved changes, instead raise a 
" dialogue asking if you wish to save changed files
set confirm
" Reload file when there is no local changes and the file has been changed
" externally
set autoread


" {{{1 Tags

set tags+=~/.tags
set tags+=./.git/tags
set tags+=./tags


" {{{1 Python Hosts

if has('nvim')
    if isdirectory(expand('~') . '/.virtualenvs/neovim2/bin/python')
        let g:python_host_prog = expand('~') . '/.virtualenvs/neovim2/bin/python'
    endif
    if isdirectory(expand('~') . '/.virtualenvs/neovim3/bin/python')
        let g:python3_host_prog = expand('~') . '/.virtualenvs/neovim3/bin/python'
    endif
endif


" {{{1 Backups

" Put the backup files in the temporary folder
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup


" {{{1 Buffers & Tabs

set hidden
