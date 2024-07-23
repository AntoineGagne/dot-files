return {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      local builtins = require 'telescope.builtin'
      vim.keymap.set({ 'n', 'v' }, '<leader>bls', function()
        builtins.buffers {
          show_all_buffers = false,
          ignore_current_buffer = true,
          sort_mru = true,
        }
      end, { remap = false })
      vim.keymap.set({ 'n', 'v' }, '<leader>mls', builtins.marks, { remap = false })
      vim.keymap.set({ 'n', 'v' }, '<leader>/', builtins.current_buffer_fuzzy_find, { remap = false })
      vim.keymap.set({ 'n', 'v' }, '<leader>ls', builtins.git_files, { remap = false })
    end,
  },
}
