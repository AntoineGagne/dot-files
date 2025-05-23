local M = {}

local function with(f, config)
  return function(options)
    options = options or {}
    return f(vim.tbl_deep_extend('force', options, config))
  end
end

local defaults = {
  implementations = vim.lsp.buf.implementation,
  declaration = vim.lsp.buf.declaration,
  definitions = vim.lsp.buf.definition,
  document_symbols = vim.lsp.buf.document_symbol,
  type_definition = vim.lsp.buf.type_definition,
  reference = vim.lsp.buf.references,
  hover = with(vim.lsp.buf.hover, { border = 'rounded', silent = false }),
  signature_help = with(vim.lsp.buf.signature_help, { border = 'rounded', silent = false }),
  workspace = {
    add = vim.lsp.buf.add_workspace_folder,
    remove = vim.lsp.buf.remove_workspace_folder,
    list = function()
      vim.print(vim.lsp.buf.list_workspace_folders())
    end,
  },
  rename = vim.lsp.buf.rename,
}

function M.new(overrides)
  if overrides == nil then
    overrides = {}
  end

  return vim.tbl_deep_extend('force', defaults, overrides)
end

function M.with_telescope(settings)
  local telescope_builtins = require('telescope.builtin')
  return vim.tbl_deep_extend('force', settings, {
    definitions = telescope_builtins.lsp_definitions,
    implementations = telescope_builtins.lsp_implementations,
    document_symbols = telescope_builtins.lsp_document_symbols,
    type_definition = telescope_builtins.lsp_type_definitions,
    reference = telescope_builtins.lsp_references,
  })
end

function M.with_snacks(settings)
  local snacks = require('snacks.picker')
  return vim.tbl_deep_extend('force', settings, {
    definitions = snacks.lsp_definitions,
    implementations = snacks.lsp_implementations,
    document_symbols = snacks.lsp_document_symbols,
    type_definition = snacks.lsp_type_definitions,
    reference = snacks.lsp_references,
  })
end

return M
