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
        userDictPath = '~/.local/state/harper-ls',
      },
    },
  }),
  hie = Setup.create({}),
  hls = Setup.create({}),
  lua_ls = Setup.create({
    callbacks = {
      on_init = function(client)
        local path = client.workspace_folders[1].name
        local luarc_json = path .. '/.luarc.json'
        local luarc_jsonc = path .. '/.luarc.jsonc'
        if vim.loop.fs_stat(luarc_json) or vim.loop.fs_stat(luarc_jsonc) then
          return
        end

        if not client.config.settings.Lua then
          client.config.settings['Lua'] = {}
        end

        client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
          runtime = {
            -- Tell the language server which version of Lua you're using
            -- (most likely LuaJIT in the case of Neovim)
            version = 'LuaJIT',
          },
          -- Make the server aware of Neovim runtime files
          workspace = {
            checkThirdParty = true,
            library = {
              vim.env.VIMRUNTIME,
              -- Depending on the usage, you might want to add additional paths here.
              -- "${3rd}/luv/library"
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

local signature_help = with(vim.lsp.buf.signature_help, { border = 'rounded', silent = false })
local hover = with(vim.lsp.buf.hover, { border = 'rounded', silent = false })

return {
  to_enable = to_enable,

  on_attach = function(client, bufnr)
    -- Mappings.
    local opts = { remap = false, silent = true, buffer = bufnr }

    -- See also `lsp-defaults.txt`
    local keymappings = {
      ['gD'] = { '<Cmd>lua vim.lsp.buf.declaration()<CR>', 'Show symbol declaration' },
      ['gd'] = {
        function()
          require('telescope.builtin').lsp_definitions()
        end,
        'Show symbol definition',
      },
      ['gri'] = {
        function()
          require('telescope.builtin').lsp_implementations()
        end,
        'Show symbol implementation',
      },
      ['gO'] = {
        function()
          require('telescope.builtin').lsp_document_symbols()
        end,
        'Show symbol implementation',
      },
      ['<leader>wa'] = { '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', 'Add to workspace folder' },
      ['<leader>wr'] = { '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', 'Remove from workspace folder' },
      ['<leader>wl'] = {
        '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
        'Show the workspace folders',
      },
      ['<leader>D'] = {
        function()
          require('telescope.builtin').lsp_type_definitions()
        end,
        'Show the symbol type definition',
      },
      ['<leader>rn'] = { '<cmd>lua vim.lsp.buf.rename()<CR>', 'Rename the symbol under the cursor' },
      ['<leader>ca'] = { '<cmd>lua vim.lsp.buf.code_action()<CR>', 'Show the code action menu' },
      ['grr'] = {
        function()
          require('telescope.builtin').lsp_references()
        end,
        'Show the symbol references',
      },
      ['K'] = { hover, 'Show symbol details' },
      ['<C-k>'] = { signature_help, 'Show signature help' },
    }
    for keys, mapping in pairs(keymappings) do
      local function_, description = unpack(mapping)
      vim.keymap.set('n', keys, function_, vim.tbl_deep_extend('force', opts, { desc = description }))
    end

    vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })

    -- Set some keybinds conditional on server capabilities
    if client.server_capabilities.document_formatting then
      buf_set_keymap('n', '<leader>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
    end

    if client.server_capabilities.document_range_formatting then
      buf_set_keymap('v', '<leader>f', '<cmd>lua vim.lsp.buf.range_formatting()<CR>', opts)
    end

    -- Set autocommands conditional on server_capabilities
    if client.server_capabilities.document_highlight then
      vim.api.nvim_exec2(
        [[
      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      augroup lsp_document_highlight
      autocmd! * <buffer>
      autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
      autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
      ]],
        { output = false }
      )
    end
  end,
}
