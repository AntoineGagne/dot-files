local templates = vim.api.nvim_create_augroup("templates", { clear = true })
vim.api.nvim_create_autocmd(
  "BufNewFile",
  {
    command = "silent! 0r $HOME/.config/nvim/templates/skeleton.%:e",
    group = templates
  }
)

local ui = vim.api.nvim_create_augroup("ui", { clear = true })
vim.api.nvim_create_autocmd(
  "ColorScheme",
  {
    command = "highlight Normal ctermbg=none guibg=none",
    group = ui
  }
)
