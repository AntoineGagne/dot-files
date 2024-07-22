vim.cmd("colorscheme gruvbox")

vim.o.encoding = 'utf-8'
vim.o.autoread = true
vim.o.fileformat = 'unix'
vim.o.fileformats = 'unix,dos,mac'
vim.o.formatoptions = 'tcroqnj'

vim.opt.tags:append({
  '~/.local/state/nvim/tags',
  './.git/tags',
  './tags'
})

-- Put the backup files in the temporary folder
vim.o.backup = true
-- The directory where to put the backups
vim.o.backupdir = '~/.local/state/nvim/tmp'
-- Skip creating backups for the files matching the following patterns
vim.opt.backupskip = { '/tmp/*,/private/tmp/*' }

-- The directory where to save the swap files
vim.o.directory = '~/.local/state/nvim/swap'
vim.o.writebackup = true

-- Keep undo even after closing the file
vim.o.undofile = true
-- Set the undo directory
vim.o.undodir = '~/.local/state/nvim/.undo'
-- Set the maximum number of undo that can be undone
vim.o.undolevels = 1000
-- Set the maximum number of lines to save for undo on a buffer reload
vim.o.undoreload = 10000

vim.o.hidden = true

vim.g.netrw_banner = 1
-- Where the bookmarks will be kept
vim.g.netrw_home = '~/.local/state/nvim/.bookmarks/'
-- Tree style listing
vim.g.netrw_liststyle = 3
-- Human readable file size
vim.g.netrw_sizestyle = 'H'
-- Sort options
-- 'i' will ignore letter case
vim.g.netrw_sort_options = 'i'
