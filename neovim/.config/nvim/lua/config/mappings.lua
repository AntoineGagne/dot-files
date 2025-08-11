-- Move vertically by visual line
vim.keymap.set({ 'n', 'v' }, 'j', 'gj', {
  remap = false,
  desc = 'Move down by visual line',
})
vim.keymap.set({ 'n', 'v' }, 'k', 'gk', {
  remap = false,
  desc = 'Move up by visual line',
})

-- Move to beginning/end of line
-- Switches `^` and `0`
vim.keymap.set({ 'n', 'v' }, '0', 'g^', { remap = false })
vim.keymap.set({ 'n', 'v' }, '^', 'g0', { remap = false })
vim.keymap.set({ 'n', 'v' }, '$', 'g$', { remap = false })

-- Fix an issue with nvim-cmp
-- vim.keymap.set('c', '<tab>', '<C-z>', { silent = false })

-- Map leader key to space
-- vim.keymap.set({'n', 'v', 'i'}, '<Space>', '<leader>')
