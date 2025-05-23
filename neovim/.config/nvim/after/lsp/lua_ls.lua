return {
  --- @param client vim.lsp.Client
  --- @param init_result lsp.InitializeResult
  on_init = function(client, init_result)
    if client.workspace_folders then
      local path = client.workspace_folders[1].name
      if
        path ~= vim.fn.stdpath('config')
        and (vim.loop.fs_stat(path .. '/.luarc.json') or vim.loop.fs_stat(path .. '/.luarc.jsonc'))
      then
        return
      end
    end

    if not client.config.settings then
      client.config.settings = {}
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
}
