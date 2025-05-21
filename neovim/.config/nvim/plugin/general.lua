vim.o.encoding = 'utf-8'
vim.o.autoread = true
vim.o.fileformat = 'unix'
vim.o.fileformats = 'unix,dos,mac'
vim.o.formatoptions = 'tcroqnj'

vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_python3_provider = 0

local state_directory = vim.fn.stdpath('state')
if type(state_directory) == 'table' then
  state_directory = state_directory[0]
end

--- @param ... string Path fragments to join and expand
--- @return string
local function expand_directory(...)
  return vim.fn.expand(vim.fs.joinpath(...)) .. '//'
end

vim.opt.tags:append({
  expand_directory(state_directory, 'nvim', 'tags'),
  './.git/tags',
  './tags',
})

-- Put the backup files in the temporary folder
vim.o.backup = true
-- The directory where to put the backups
vim.o.backupdir = expand_directory(state_directory, 'nvim', 'backup')
-- Skip creating backups for the files matching the following patterns
vim.opt.backupskip = { '/tmp/*', '/private/tmp/*' }

-- The directory where to save the swap files
vim.o.directory = expand_directory(state_directory, 'nvim', 'swap')
vim.o.writebackup = true

-- Keep undo even after closing the file
vim.o.undofile = true
-- Set the undo directory
vim.o.undodir = expand_directory(state_directory, 'nvim', '.undo')
-- Set the maximum number of undo that can be undone
vim.o.undolevels = 1000
-- Set the maximum number of lines to save for undo on a buffer reload
vim.o.undoreload = 10000

vim.o.hidden = true

-- Enable folding
vim.o.foldenable = true
-- Open most folds by default
vim.o.foldlevelstart = 10
-- 10 nested fold max
vim.o.foldnestmax = 10
-- Fold based on indent level
-- vim.o.foldmethod = 'indent'

-- Case insensitive search
vim.o.ignorecase = true
-- Case sensitive search when using capitals
vim.o.smartcase = true
-- Search as characters are entered
vim.o.incsearch = true
-- Do not highlight matches
vim.o.hlsearch = false

if vim.fn.executable('ag') then
  vim.o.grepprg = 'ag --nogroup --nocolor'
end

if vim.fn.executable('pt') then
  vim.o.grepprg = 'pt --nogroup --nocolor --ignore-case'
end

if vim.fn.executable('rg') then
  vim.o.grepprg = 'rg --vimgrep --no-heading'
  vim.o.grepformat = '%f:%l:%c:%m,%f:%l:%m'
end

-- Shows the effects of a command incrementally, as you type
vim.o.inccommand = 'nosplit'

-- Show the current editing mode
vim.o.showmode = false

-- Enables spell check
vim.o.spell = true
-- Enables the english and the french spell checker
vim.opt.spelllang = { 'en', 'fr' }

-- Show spaces visually
vim.o.list = true
-- Display spaces as dots and tabs as arrows
vim.opt.listchars = {
  space = '·',
  tab = '→ ',
  nbsp = '␣',
}
-- Allow backspacing over autoindent, line breaks and start of insert action
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.o.autoindent = true
-- Number of visual spaces per TAB
vim.o.tabstop = 4
-- Number of spaces in tab when editing
vim.o.softtabstop = 4
-- Number of spaces inserted for indentation
vim.o.shiftwidth = 4
-- Replace tabs by spaces
vim.o.expandtab = true
-- Make tab insert indents instead of tabs at the beginning of a line
vim.o.smarttab = true

vim.o.foldmethod = 'expr'
-- Use treesitter for folding
vim.o.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
vim.o.foldenable = false

-- Load filetype-specific indent files
vim.cmd([[filetype indent on]])
-- Redraw only when we need to
vim.o.lazyredraw = true
-- Use visual bell instead of beeping when doing something wrong
vim.o.visualbell = true

-- Enable syntax processing
vim.cmd([[syntax enable]])
vim.o.conceallevel = 2
vim.g.tex_conceal = 'adgms'

-- Set the terminal's title
vim.o.title = true
-- Show line numbers
vim.o.number = true
-- Separators
vim.opt.fillchars = {
  vert = '│',
  fold = '─',
}
-- Display the cursor position on the last line of the screen or in the status
-- line of a window
vim.o.ruler = true
-- Always display the status line, even if only one window is displayed
vim.o.laststatus = 2

-- Highlight matching [{()}]
vim.o.showmatch = true
-- Highlight current line
vim.o.cursorline = true
-- Use a bar-shaped cursor for insert mode, even through tmux.
vim.o.guicursor =
  'n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175'
-- Set the number of screen lines above and below the cursor
vim.o.scrolloff = 4

-- Set the command window height
-- vim.o.cmdheight = 1
-- Show command in bottom bar
-- vim.o.showcmd = true
-- Visual autocomplete for command menu
-- vim.o.wildmenu = true

vim.g.netrw_banner = 1
-- Where the bookmarks will be kept
vim.g.netrw_home = vim.fn.expand(vim.fs.joinpath(state_directory, 'nvim', '.bookmarks'))
-- Tree style listing
vim.g.netrw_liststyle = 3
-- Human readable file size
vim.g.netrw_sizestyle = 'H'
-- Sort options
-- 'i' will ignore letter case
vim.g.netrw_sort_options = 'i'

-- see `exrc`
vim.g.exrc = false

-- For transparency
vim.cmd([[
highlight Normal guibg=none
highlight NonText guibg=none
highlight Normal ctermbg=none
highlight NonText ctermbg=none
]])

vim.diagnostic.config({
  virtual_text = true,
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '❌',
      [vim.diagnostic.severity.WARN] = '⚠️',
      [vim.diagnostic.severity.INFO] = 'ⓘ',
      [vim.diagnostic.severity.HINT] = '💡',
    },
  },
})
