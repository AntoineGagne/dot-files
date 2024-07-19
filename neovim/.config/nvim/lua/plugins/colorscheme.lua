return {
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    config = true,
    lazy = true,
    opts = {
      -- add neovim terminal colors
      terminal_colors = true,
      undercurl = true,
      underline = true,
      bold = true,
      italic = {
        strings = true,
        emphasis = true,
        comments = true,
        operators = false,
        folds = true,
      },
      strikethrough = true,
      invert_selection = false,
      invert_signs = false,
      invert_tabline = false,
      invert_intend_guides = false,
      -- invert background for search, diffs, statuslines and errors
      inverse = true,
      -- can be "hard", "soft" or empty string
      contrast = "",
      palette_overrides = {},
      overrides = {},
      dim_inactive = false,
      transparent_mode = false,
    }
  }
}
