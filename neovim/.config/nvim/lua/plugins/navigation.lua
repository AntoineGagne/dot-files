return {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      local builtins = require('telescope.builtin')
      vim.keymap.set({ 'n', 'v' }, '<leader>bls', function()
        builtins.buffers({
          show_all_buffers = true,
          select_current = false,
          sort_mru = true,
        })
      end, {
        remap = false,
        desc = 'Search buffers',
      })
      vim.keymap.set({ 'n', 'v' }, '<leader>mls', builtins.marks, {
        remap = false,
        desc = 'Search marks',
      })
      vim.keymap.set({ 'n', 'v' }, '<leader>/', builtins.current_buffer_fuzzy_find, {
        remap = false,
        desc = 'Search the current buffer',
      })
      vim.keymap.set({ 'n', 'v' }, '<leader>ls', builtins.git_files, {
        remap = false,
        desc = 'Search git files',
      })
    end,
  },
}
