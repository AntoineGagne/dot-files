return {
  -- Treesitter is a new parser generator tool that we can
  -- use in Neovim to power faster and more accurate
  -- syntax highlighting.
  {
    'nvim-treesitter/nvim-treesitter',
    branch = 'main',
    build = ':TSUpdate',
    lazy = false,
    config = function(_, options)
      local filetypes = require('config.constants').TREESITTER_ENABLED_FILETYPES
      require('nvim-treesitter').setup(options)
      require('nvim-treesitter').install(filetypes)
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    dependencies = { 'nvim-treesitter' },
    branch = 'main',
    config = function()
      -- Disable entire built-in ftplugin mappings to avoid conflicts.
      -- See https://github.com/neovim/neovim/tree/master/runtime/ftplugin for built-in ftplugins.
      -- Or, disable per filetype (add as you like)
      -- vim.g.no_python_maps = true
      -- vim.g.no_ruby_maps = true
      -- vim.g.no_rust_maps = true
      -- vim.g.no_go_maps = true
      vim.g.no_plugin_maps = true

      require('nvim-treesitter-textobjects').setup({
        select = {
          enable = true,

          -- Automatically jump forward to textobj, similar to targets.vim
          lookahead = true,

          -- You can choose the select mode (default is charwise 'v')
          --
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg '@function.inner'
          -- * method: eg 'v' or 'o'
          -- and should return the mode ('v', 'V', or '<c-v>') or a table
          -- mapping query_strings to modes.
          selection_modes = {
            ['@parameter.outer'] = 'v', -- charwise
            ['@function.outer'] = 'V', -- linewise
            ['@class.outer'] = 'V', -- blockwise
          },
          -- If you set this to `true` (default is `false`) then any textobject is
          -- extended to include preceding or succeeding whitespace. Succeeding
          -- whitespace has priority in order to act similarly to eg the built-in
          -- `ap`.
          --
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg '@function.inner'
          -- * selection_mode: eg 'v'
          -- and should return true or false
          include_surrounding_whitespace = false,
        },
      })

      local keys_by_queries = {
        -- You can use the capture groups defined in textobjects.scm
        ['af'] = { query = '@function.outer', desc = 'Select outer part of a function region' },
        ['if'] = { query = '@function.inner', desc = 'Select inner part of a function region' },
        ['aC'] = { query = '@class.outer', desc = 'Select outer part of a class region' },
        -- You can optionally set descriptions to the mappings (used in the desc parameter of
        -- nvim_buf_set_keymap) which plugins like which-key display
        ['iC'] = { query = '@class.inner', desc = 'Select inner part of a class region' },
        ['ac'] = { query = '@conditional.outer', desc = 'Select outer part of a conditional region' },
        ['ic'] = { query = '@conditional.inner', desc = 'Select inner part of a conditional region' },
        ['ae'] = { query = '@block.outer', desc = 'Select outer part of a block region' },
        ['ie'] = { query = '@block.inner', desc = 'Select inner part of a block region' },
        ['al'] = { query = '@loop.outer', desc = 'Select outer part of a loop region' },
        ['il'] = { query = '@loop.inner', desc = 'Select inner part of a loop region' },
        ['as'] = { query = '@statement.outer', desc = 'Select outer part of a statement region' },
        ['is'] = { query = '@statement.inner', desc = 'Select inner part of a statement region' },
        ['ad'] = { query = '@comment.outer', desc = 'Select outer part of a comment region' },
        ['id'] = { query = '@comment.inner', desc = 'Select inner part of a comment region' },
        ['am'] = { query = '@call.outer', desc = 'Select outer part of a call region' },
        ['im'] = { query = '@call.inner', desc = 'Select inner part of a call region' },
        -- You can also use captures from other query groups like `locals.scm`
        -- ['as'] = { query = '@scope', query_group = 'locals', desc = 'Select language scope' },
      }
      for key, query in pairs(keys_by_queries) do
        vim.keymap.set({ 'x', 'o' }, key, function()
          require('nvim-treesitter-textobjects.select').select_textobject(query.query, 'textobjects')
        end, {
          desc = query.desc,
        })
      end
    end,
  },
}
