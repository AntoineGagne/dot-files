local templates = vim.api.nvim_create_augroup('templates', { clear = true })
vim.api.nvim_create_autocmd('BufNewFile', {
  command = 'silent! 0r $HOME/.config/nvim/templates/skeleton.%:e',
  group = templates,
})

local ui = vim.api.nvim_create_augroup('ui', { clear = true })
vim.api.nvim_create_autocmd('ColorScheme', {
  command = 'highlight Normal ctermbg=none guibg=none',
  group = ui,
})

local snacks_rename = vim.api.nvim_create_augroup('snacks-rename', { clear = true })
vim.api.nvim_create_autocmd('User', {
  pattern = 'OilActionsPost',
  callback = function(event)
    if event.data.actions.type == 'move' then
      Snacks.rename.on_rename_file(event.data.actions.src_url, event.data.actions.dest_url)
    end
  end,
  group = snacks_rename,
})
