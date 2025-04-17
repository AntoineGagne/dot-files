local Setup = (function()
  local defaults = {
    command = nil,
    settings = nil,
    enabled = true,

    callbacks = {
      on_attach = function(client, bufnr) end,
      on_init = function(client) end,
    },
  }

  return {
    create = function(overrides)
      return vim.tbl_deep_extend('force', defaults, overrides)
    end,
  }
end)()

local to_enable = {
  bashls = Setup.create({}),
  clangd = Setup.create({}),
  cmake = Setup.create({}),
  cssls = Setup.create({}),
  elmls = Setup.create({}),
  erlangls = Setup.create({}),
  elixirls = Setup.create({
    command = { '/home/a.gagne/.local/bin/elixir-ls' },
  }),
  harper_ls = Setup.create({
    settings = {
      ['harper-ls'] = {
        userDictPath = '~/.local/state/harper-ls/dictionary',
      },
    },
  }),
  hie = Setup.create({}),
  hls = Setup.create({}),
  lua_ls = Setup.create({
    callbacks = {
      on_init = function(client)
        if client.workspace_folders then
          local path = client.workspace_folders[1].name
          if
            path ~= vim.fn.stdpath('config')
            and (vim.loop.fs_stat(path .. '/.luarc.json') or vim.loop.fs_stat(path .. '/.luarc.jsonc'))
          then
            return
          end
        end

        if not client.config.settings.Lua then
          client.config.settings['Lua'] = {}
        end

        client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
          runtime = {
            version = 'LuaJIT',
          },

          -- Make the server aware of Neovim runtime files
          workspace = {
            checkThirdParty = false,
            library = {
              '$VIMRUNTIME',
              '${3rd}/luv/library',
              '$XDG_DATA_HOME/nvim/lazy',
              -- "${3rd}/busted/library",
            },
            -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
            -- library = vim.api.nvim_get_runtime_file("", true)
          },
        })
      end,
    },
  }),
  purescriptls = Setup.create({}),
  basedpyright = Setup.create({}),
  pyright = Setup.create({
    enabled = false,
    callbacks = {
      on_attach = function(client, bufnr)
        client.server_capabilities.hoverProvider = false
      end,
    },
  }),
  pylsp = Setup.create({
    enabled = false,
    callbacks = {
      on_attach = function(client, bufnr)
        client.server_capabilities.definitionProvider = false
        client.server_capabilities.documentSymbolProvider = false
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.renameProvider = false
        client.server_capabilities.referencesProvider = false
      end,
    },
  }),
  ruff = Setup.create({}),
  rust_analyzer = Setup.create({
    settings = {
      ['rust-analyzer'] = {
        checkOnSave = {
          command = 'clippy',
        },
        imports = {
          granularity = {
            group = 'module',
          },
          prefix = 'self',
        },
        cargo = {
          buildScripts = {
            enable = true,
          },
        },
        procMacro = {
          enable = true,
        },
      },
    },
  }),
  texlab = Setup.create({}),
  ts_ls = Setup.create({}),
}

local function with(f, config)
  return function(options)
    options = options or {}
    return f(vim.tbl_deep_extend('force', options, config))
  end
end

local GlobalSettings = (function()
  local defaults = {
    callbacks = {
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
    },
  }

  local function create(overrides)
    return vim.tbl_deep_extend('force', defaults, overrides)
  end

  return {
    create = create,

    with_telescope = function(settings)
      local telescope_builtins = require('telescope.builtin')
      return vim.tbl_deep_extend('force', settings, {
        callbacks = {
          definitions = telescope_builtins.lsp_definitions,
          implementations = telescope_builtins.lsp_implementations,
          document_symbols = telescope_builtins.lsp_document_symbols,
          type_definition = telescope_builtins.lsp_type_definitions,
          reference = telescope_builtins.lsp_references,
        },
      })
    end,
  }
end)()

return {
  to_enable = to_enable,
  global_settings = GlobalSettings,
  on_attach = function(settings, client, bufnr)
    -- Mappings.
    local opts = { remap = false, silent = true, buffer = bufnr }

    local keymappings = {
      ['gD'] = { settings.callbacks.declaration, 'Show symbol declaration' },
      ['gd'] = { settings.callbacks.definitions, 'Show symbol definition' },
      ['gri'] = { settings.callbacks.implementations, 'Show symbol implementation' },
      ['gO'] = { settings.callbacks.document_symbols, 'Show symbol implementation' },
      ['<leader>wa'] = { settings.callbacks.workspace.add, 'Add to workspace folder' },
      ['<leader>wr'] = { settings.callbacks.workspace.remove, 'Remove from workspace folder' },
      ['<leader>wl'] = { settings.callbacks.workspace.list, 'Show the workspace folders' },
      ['<leader>D'] = { settings.callbacks.type_definition, 'Show the symbol type definition' },
      ['<leader>rn'] = { settings.callbacks.rename, 'Rename the symbol under the cursor' },
      ['grr'] = { settings.callbacks.reference, 'Show the symbol references' },
      ['K'] = { settings.callbacks.hover, 'Show symbol details' },
      ['<C-k>'] = { settings.callbacks.signature_help, 'Show signature help' },
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
  end,
}
