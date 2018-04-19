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


" {{{1 Backups

" Put the backup files in the temporary folder
set backup
call functions#CreateDirectoryIfItDoesNotExists(expand('$HOME') . '/.vim/.tmp')
" The directory where to put the backups
set backupdir=~/.vim/.tmp
" Skip creating backups for the files matching the following patterns
set backupskip=/tmp/*,/private/tmp/*

" {{{1 Swap
" The directory where to save the swap files
call functions#CreateDirectoryIfItDoesNotExists(expand('$HOME') . '/.vim/.swap')
set directory=~/.vim/.swap
set writebackup

" {{{1 Undo
" Keep undo even after closing the file
set undofile
call functions#CreateDirectoryIfItDoesNotExists(expand('~') . '/.vim/.undo')
" Set the undo directory
set undodir=~/.vim/.undo
" Set the maximum number of undo that can be undone
set undolevels=1000
" Set the maximum number of lines to save for undo on a buffer reload
set undoreload=10000

" {{{1 Buffers & Tabs

set hidden

" {{{1 Netrw

let g:netrw_banner = 1
" Where the bookmarks will be kept
call functions#CreateDirectoryIfItDoesNotExists(expand('~') . '/.vim/.bookmarks')
let g:netrw_home = expand('~') . '/.vim/.bookmarks/'
" Tree style listing
let g:netrw_liststyle = 3
" Human readable file size
let g:netrw_sizestyle = 'H'
" Sort options
" 'i' will ignore letter case
let g:netrw_sort_options = 'i'
