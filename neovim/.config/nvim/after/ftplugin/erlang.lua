vim.g.erlang_highlight_special_atoms = 1

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
