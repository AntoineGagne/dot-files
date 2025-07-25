return {
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
    },
    opts = {

      options = {
        icons_enabled = false,
        theme = 'auto',
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        disabled_filetypes = {
          statusline = {},
          winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
          statusline = 1000,
          tabline = 1000,
          winbar = 1000,
        },
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch' },
        lualine_c = { 'filename' },
        lualine_x = { 'encoding', 'fileformat', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location', 'diagnostics' },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { 'filename' },
        lualine_x = { 'location' },
        lualine_y = {},
        lualine_z = {},
      },
      tabline = {},
      inactive_winbar = {},
      extensions = {},
    },
  },
  {
    'shellRaining/hlchunk.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require('hlchunk').setup({
        chunk = {
          enable = true,
        },
        indent = {
          enable = true,
        },
      })
    end,
  },
  {
    '3rd/image.nvim',
    build = false,
    opts = {
      processor = 'magick_cli',
      integrations = {
        markdown = {
          -- defaults to false
          only_render_image_at_cursor = true,
          -- "popup" or "inline", defaults to "popup"
          only_render_image_at_cursor_mode = 'popup',
        },
        neorg = {
          -- defaults to false
          only_render_image_at_cursor = true,
          -- "popup" or "inline", defaults to "popup"
          only_render_image_at_cursor_mode = 'popup',
        },
      },
    },
  },
}
