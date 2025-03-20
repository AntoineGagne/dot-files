return {
  to_enable = {
    bashls = { nil, nil, nil },
    clangd = { nil, nil, nil },
    cmake = { nil, nil, nil },
    cssls = { nil, nil, nil },
    elmls = { nil, nil, nil },
    erlangls = { nil, nil, nil },
    elixirls = {
      { '/home/a.gagne/.local/bin/elixir-ls' },
      nil,
      nil,
    },
    harper_ls = {
      nil,
      {
        ['harper-ls'] = {
          userDictPath = '~/.local/state/harper-ls',
        },
      },
      nil,
    },
    hie = { nil, nil, nil },
    hls = { nil, nil, nil },
    lua_ls = {
      nil,
      nil,
      function(client)
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
    purescriptls = { nil, nil, nil },
    pyright = { nil, nil, nil },
    rust_analyzer = {
      nil,
      {
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
      nil,
    },
    texlab = { nil, nil, nil },
    ts_ls = { nil, nil, nil },
  },

  on_attach = function(client, bufnr)
    -- Mappings.
    local opts = { remap = false, silent = true, buffer = bufnr }
    -- See also `lsp-defaults.txt`
    local keymappings = {
      ['gD'] = { '<Cmd>lua vim.lsp.buf.declaration()<CR>', 'Show symbol declaration' },
      ['gd'] = { '<Cmd>lua vim.lsp.buf.definition()<CR>', 'Show symbol definition' },
      ['gi'] = { '<cmd>lua vim.lsp.buf.implementation()<CR>', 'Show symbol implementation' },
      ['K'] = { '<Cmd>lua vim.lsp.buf.hover()<CR>', 'Show symbol details' },
      ['<C-k>'] = { '<cmd>lua vim.lsp.buf.signature_help()<CR>', 'Show signature help' },
      ['<leader>wa'] = { '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', 'Add to workspace folder' },
      ['<leader>wr'] = { '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', 'Remove from workspace folder' },
      ['<leader>wl'] = {
        '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
        'Show the workspace folders',
      },
      ['<leader>D'] = { '<cmd>lua vim.lsp.buf.type_definition()<CR>', 'Show the symbol type definition' },
      ['<leader>rn'] = { '<cmd>lua vim.lsp.buf.rename()<CR>', 'Rename the symbol under the cursor' },
      ['<leader>ca'] = { '<cmd>lua vim.lsp.buf.code_action()<CR>', 'Show the code action menu' },
      ['gr'] = { '<cmd>lua vim.lsp.buf.references()<CR>', 'Show the symbol references' },
      ['<leader>dK'] = {
        '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',
        'Show the diagnostic under the cursor',
      },
      ['<leader>dk'] = { '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', 'Go to the previous diagnostic' },
      ['<leader>dj'] = { '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', 'Go to the next diagnostic' },
      ['<leader>dsl'] = { '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', 'Set the location list' },
    }
    for keys, mapping in pairs(keymappings) do
      function_, description = unpack(mapping)
      vim.keymap.set('n', keys, function_, vim.tbl_deep_extend('force', opts, { desc = description }))
    end

    vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })

    if client.resolved_capabilities then
      -- Set some keybinds conditional on server capabilities
      if client.resolved_capabilities.document_formatting then
        buf_set_keymap('n', '<leader>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
      end
      if client.resolved_capabilities.document_range_formatting then
        buf_set_keymap('v', '<leader>f', '<cmd>lua vim.lsp.buf.range_formatting()<CR>', opts)
      end

      -- Set autocommands conditional on server_capabilities
      if client.resolved_capabilities.document_highlight then
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
    end
  end,
}
