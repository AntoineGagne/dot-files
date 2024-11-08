return {
  {
    'OXY2DEV/markview.nvim',
    lazy = false, -- Recommended
    -- ft = "markdown" -- If you decide to lazy-load anyway

    dependencies = {
      -- You will not need this if you installed the
      -- parsers manually
      -- Or if the parsers are in your $RUNTIMEPATH
      'nvim-treesitter/nvim-treesitter',
      'nvim-tree/nvim-web-devicons',
    },
  },
  {
    'nvim-neorg/neorg',
    lazy = false,
    version = '*',
    config = true,
  },
  -- {
  --   'MeanderingProgrammer/markdown.nvim',
  --   main = 'render-markdown',
  --   opts = {},
  --   -- Only needed if you have another plugin named markdown.nvim
  --   -- name = 'render-markdown',
  --   -- if you use the mini.nvim suite
  --   dependencies = {
  --     'nvim-treesitter/nvim-treesitter',
  --     'echasnovski/mini.nvim',
  --   },
  --   ---- if you use standalone mini plugins
  --   -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' },
  --   ---- if you prefer nvim-web-devicons
  --   -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' },
  -- },
}
