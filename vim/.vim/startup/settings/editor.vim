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
" Set the default file mode
set fileformat=unix
" Fallback file mode
set fileformats=unix,dos,mac

" {{{1 Formatting
" See :help formatoptions and :help fo-table
set formatoptions=tcroqnj


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
call CreateDirectoryIfItDoesNotExists(expand('$HOME') . '/.tmp')
" The directory where to put the backups
set backupdir=~/.tmp
" Skip creating backups for the files matching the following patterns
set backupskip=/tmp/*,/private/tmp/*

" {{{1 Swap
" The directory where to save the swap files
call CreateDirectoryIfItDoesNotExists(expand('$HOME') . '/.swap')
set directory=~/.swap
set writebackup

" {{{1 Undo
" Keep undo even after closing the file
set undofile
call CreateDirectoryIfItDoesNotExists(expand('~') . '/.undo')
" Set the undo directory
set undodir=~/.undo
" Set the maximum number of undo that can be undone
set undolevels=1000
" Set the maximum number of lines to save for undo on a buffer reload
set undoreload=10000

" {{{1 Buffers & Tabs

set hidden
