local Config = {}
Config.__index = Config

function Config.new(handler)
  --- @class Config
  local self = {
    handler = handler,
  }

  setmetatable(self, Config)
  return self
end

--- @param client vim.lsp.Client
--- @param bufnr integer
function Config:on_attach(client, bufnr)
  -- Mappings.
  local opts = { remap = false, silent = true, buffer = bufnr }
  local handler = self.handler

  local keymappings = {
    ['gD'] = { handler.declaration, 'Show symbol declaration' },
    ['gd'] = { handler.definitions, 'Show symbol definition' },
    ['gri'] = { handler.implementations, 'Show symbol implementation' },
    ['gO'] = { handler.document_symbols, 'Show symbol implementation' },
    ['<leader>wa'] = { handler.workspace.add, 'Add to workspace folder' },
    ['<leader>wr'] = { handler.workspace.remove, 'Remove from workspace folder' },
    ['<leader>wl'] = { handler.workspace.list, 'Show the workspace folders' },
    ['<leader>D'] = { handler.type_definition, 'Show the symbol type definition' },
    ['<leader>rn'] = { handler.rename, 'Rename the symbol under the cursor' },
    ['grr'] = { handler.reference, 'Show the symbol references' },
    ['K'] = { handler.hover, 'Show symbol details' },
    ['<C-k>'] = { handler.signature_help, 'Show signature help' },
  }

  for keys, mapping in pairs(keymappings) do
    local function_, description = unpack(mapping)
    vim.keymap.set('n', keys, function_, vim.tbl_deep_extend('force', opts, { desc = description }))
  end

  vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })

  -- Set autocommands conditional on server_capabilities
  if client.server_capabilities.document_highlight then
    local lsp_document_highlight = vim.api.nvim_create_augroup('lsp_document_highlight', { clear = true })
    vim.api.nvim_clear_autocmds({ event = '*', pattern = '*', group = lsp_document_highlight })
    vim.api.nvim_create_autocmd('CursorHold', {
      callback = vim.lsp.buf.document_highlight,
      desc = 'Send request to the server to resolve document highlights for the current text document position.',
      buffer = bufnr,
      group = lsp_document_highlight,
    })
    vim.api.nvim_create_autocmd('CursorMoved', {
      callback = vim.lsp.buf.clear_references,
      desc = 'Removes document highlights from current buffer.',
      buffer = bufnr,
      group = lsp_document_highlight,
    })
    vim.api.nvim_exec2(
      [[
      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      ]],
      { output = false }
    )
  end
end

function Config:servers()
  local servers = {}

  local lsp_files = vim.api.nvim_get_runtime_file(vim.fs.joinpath('after', 'lsp') .. '/*.lua', true)
  for _, file in ipairs(lsp_files) do
    local server_name = vim.fn.fnamemodify(file, ':t:r')
    table.insert(servers, server_name)
  end

  return servers
end

return Config
