return {
  {
    'folke/snacks.nvim',
    priority = 1000,
    lazy = false,
    ---@type snacks.Config
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      bigfile = { enabled = true },
      dashboard = { enabled = false },
      explorer = { enabled = false },
      indent = { enabled = false },
      input = { enabled = true },
      picker = {
        enabled = true,
        config = function()
          vim.ui.select = Snacks.picker.select
        end,
      },
      notifier = { enabled = true },
      quickfile = { enabled = true },
      rename = { enabled = true },
      scope = { enabled = true },
      scroll = { enabled = false },
      statuscolumn = { enabled = true },
      words = { enabled = false },
    },
    keys = {
      {
        '<leader>bls',
        function()
          Snacks.picker.buffers()
        end,
        'Search buffers',
      },
      {
        '<leader>mls',
        function()
          Snacks.picker.marks()
        end,
        'Search marks',
      },
      {
        '<leader>/',
        function()
          Snacks.picker.lines()
        end,
        'Search the current buffer',
      },
      {
        '<leader>g/',
        function()
          Snacks.picker.grep()
        end,
        'Search within the files',
      },
      {
        '<leader>ls',
        function()
          Snacks.picker.smart()
        end,
        'Search files',
      },
      {
        '<leader>:',
        function()
          Snacks.picker.command_history()
        end,
        'Search command history',
      },
    },
  },
  {
    'nvim-telescope/telescope.nvim',
    enabled = false,
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
      vim.keymap.set({ 'n', 'v' }, '<leader>ls', builtins.find_files, {
        remap = false,
        desc = 'Search git files',
      })
    end,
  },
  {
    'AntoineGagne/telescope-nucleo-sorter.nvim',
    enabled = false,
    build = 'cargo build --release',
    config = function()
      require('telescope').load_extension('nucleo')
    end,
  },
  {
    'nvim-telescope/telescope-ui-select.nvim',
    enabled = false,
    config = function()
      require('telescope').load_extension('ui-select')
    end,
  },
  {
    'stevearc/oil.nvim',
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {
      view_options = {
        show_hidden = true,
      },
    },
    -- Optional dependencies
    dependencies = {
      {
        'echasnovski/mini.icons',
        opts = {},
      },
    },
    -- dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if you prefer nvim-web-devicons
    -- Lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
    lazy = false,
  },
}
