-- Move vertically by visual line
vim.keymap.set({'n', 'v'}, 'j', 'gj', { remap = false })
vim.keymap.set({'n', 'v'}, 'k', 'gk', { remap = false })

-- Move to beginning/end of line
-- Switches `^` and `0`
vim.keymap.set({'n', 'v'}, '0', 'g^', { remap = false })
vim.keymap.set({'n', 'v'}, '^', 'g0', { remap = false })
vim.keymap.set({'n', 'v'}, '$', 'g$', { remap = false })

-- Map leader key to space
-- vim.keymap.set({'n', 'v', 'i'}, '<Space>', '<leader>')

local builtins = require("telescope.builtin")
vim.keymap.set(
  {'n', 'v'},
  '<leader>bls',
  function()
    builtins.buffers({
      show_all_buffers = false,
      ignore_current_buffer = true,
      sort_mru = true
    })
  end,
    { remap = false })
vim.keymap.set({'n', 'v'}, '<leader>mls', builtins.marks, { remap = false })
vim.keymap.set({'n', 'v'}, '<leader>/', builtins.current_buffer_fuzzy_find, { remap = false })
vim.keymap.set({'n', 'v'}, '<leader>ls', builtins.git_files, { remap = false })
