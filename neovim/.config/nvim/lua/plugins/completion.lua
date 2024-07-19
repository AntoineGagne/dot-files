local to_enable = {
  bashls = {nil, nil, nil},
  clangd = {nil, nil, nil},
  cssls = {nil, nil, nil},
  elmls = {nil, nil, nil},
  erlangls = {nil, nil, nil},
  elixirls = {
    { "/home/a.gagne/.local/bin/elixir-ls" },
    nil,
    nil
  },
  hie = {nil, nil, nil},
  hls = {nil, nil, nil},
  lua_ls = {nil, nil, function(client)
    local path = client.workspace_folders[1].name
    local luarc_json = path..'/.luarc.json'
    local luarc_jsonc = path..'/.luarc.jsonc'
    if vim.loop.fs_stat(luarc_json) or vim.loop.fs_stat(luarc_jsonc) then
      return
    end

    if not client.config.settings.Lua then
      client.config.settings['Lua'] = {}
    end

    client.config.settings.Lua = vim.tbl_deep_extend(
    'force',
    client.config.settings.Lua,
    {
      runtime = {
        -- Tell the language server which version of Lua you're using
        -- (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT'
      },
      -- Make the server aware of Neovim runtime files
      workspace = {
        checkThirdParty = true,
        library = {
          vim.env.VIMRUNTIME
          -- Depending on the usage, you might want to add additional paths here.
          -- "${3rd}/luv/library"
          -- "${3rd}/busted/library",
        }
        -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
        -- library = vim.api.nvim_get_runtime_file("", true)
      }
    })
  end},
  purescriptls = {nil, nil, nil},
  rust_analyzer = {
    nil,
    {
      ["rust-analyzer"] = {
        imports = {
          granularity = {
            group = "module",
          },
          prefix = "self",
        },
        cargo = {
          buildScripts = {
            enable = true,
          },
        },
        procMacro = {
          enable = true
        },
      }
    },
    nil
  },
  texlab = {nil, nil, nil},
  tsserver = {nil, nil, nil},
}

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }
  local keymappings = {
    ['gD'] = '<Cmd>lua vim.lsp.buf.declaration()<CR>',
    ['gd'] = '<Cmd>lua vim.lsp.buf.definition()<CR>',
    ['K'] = '<Cmd>lua vim.lsp.buf.hover()<CR>',
    ['gi'] = '<cmd>lua vim.lsp.buf.implementation()<CR>',
    ['<C-k>'] = '<cmd>lua vim.lsp.buf.signature_help()<CR>',
    ['<space>wa'] = '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>',
    ['<space>wr'] = '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>',
    ['<space>wl'] = '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
    ['<space>D'] = '<cmd>lua vim.lsp.buf.type_definition()<CR>',
    ['<space>rn'] = '<cmd>lua vim.lsp.buf.rename()<CR>',
    ['<space>ca'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
    ['gr'] = '<cmd>lua vim.lsp.buf.references()<CR>',
    ['<space>e'] = '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',
    ['[d'] = '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>',
    [']d'] = '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>',
    ['<space>q'] = '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>'
  }
  for keys, mapping in pairs(keymappings) do
    buf_set_keymap('n', keys, mapping, opts)
  end

  if client.resolved_capabilities then
    -- Set some keybinds conditional on server capabilities
    if client.resolved_capabilities.document_formatting then
      buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    end
    if client.resolved_capabilities.document_range_formatting then
      buf_set_keymap("v", "<space>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
    end

    -- Set autocommands conditional on server_capabilities
    if client.resolved_capabilities.document_highlight then
      vim.api.nvim_exec([[
      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      augroup lsp_document_highlight
      autocmd! * <buffer>
      autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
      autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
      ]], false)
    end
  end
end

return {
  {
    'neovim/nvim-lspconfig',
  },
  {
    'hrsh7th/cmp-nvim-lsp'
  },
  {
    'hrsh7th/cmp-buffer'
  },
  {
    'hrsh7th/cmp-path'
  },
  {
    'hrsh7th/cmp-cmdline'
  },
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'cmp-buffer',
      'cmp-cmdline',
      'cmp-nvim-lsp',
      'cmp-path',
    },
    config = function()
        local cmp = require'cmp'
        cmp.setup({
          snippet = {
            -- REQUIRED - you must specify a snippet engine
            expand = function(args)
              vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
              -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
              -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
              -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
              -- vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
            end,
          },
          window = {
            -- completion = cmp.config.window.bordered(),
            -- documentation = cmp.config.window.bordered(),
          },
          mapping = cmp.mapping.preset.insert({
            ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.abort(),
            -- Accept currently selected item. Set `select` to `false` to only
            -- confirm explicitly selected items.
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
          }),
          sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'buffer' },
          })
        })

        -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
        cmp.setup.cmdline({ '/', '?' }, {
          mapping = cmp.mapping.preset.cmdline(),
          sources = {
            { name = 'buffer' }
          }
        })

        local capabilities = require("cmp_nvim_lsp").default_capabilities()
        local lspconfig = require("lspconfig")
        for server, setup in pairs(to_enable) do
          local command, config, on_init = unpack(setup)
          local lsp = {
            settings = config or {},
            on_attach = on_attach,
            handlers = {
              ['textDocument/references'] = vim.lsp.with(
              vim.lsp.handlers['textDocument/references'],
              {
                loclist = true
              }
              )
            },
            flags = {
              debounce_text_changes = 150
            },
            capabilities = capabilities
          }

          if on_init then
            lsp["on_init"] = on_init
          end

          if command then
            lsp["cmd"] = command
          end

          lspconfig[server].setup(lsp)
        end
    end,
  }
}
