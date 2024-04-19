local custom_settings = {
    bashls = {};
    clangd = {};
    cssls = {};
    elmls = {};
    erlangls = {};
    elixirls = {};
    hie = {};
    hls = {};
    purescriptls = {
       log_level = vim.lsp.protocol.MessageType.Log;
       message_level = vim.lsp.protocol.MessageType.Log;
    };
    pyls = {};
    rust_analyzer = {
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
    };
    texlab = {};
    tsserver = {};
};

local cmds = {
    elixirls = { "/home/a.gagne/.local/bin/elixir-ls" };
};

local on_init = {
  lua_ls = function(client)
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
          checkThirdParty = false,
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
  end
};

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
    configurations = custom_settings,
    on_attach = on_attach,
    cmds = cmds,
    on_init = on_init
};
