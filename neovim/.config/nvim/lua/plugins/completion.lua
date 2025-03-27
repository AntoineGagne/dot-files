return {
  {
    'neovim/nvim-lspconfig',
    dependencies = { 'saghen/blink.cmp' },
    config = function(_, opts)
      local local_config = require('lsp.config')
      local capabilities = require('blink.cmp').get_lsp_capabilities({})
      local lspconfig = require('lspconfig')
      for server, setup in pairs(local_config.to_enable) do
        local command, config, on_init = unpack(setup)
        local lsp = {
          settings = config or {},
          on_attach = local_config.on_attach,
          handlers = {
            ['textDocument/references'] = vim.lsp.with(vim.lsp.handlers['textDocument/references'], {
              loclist = true,
            }),
            ['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers['textDocument/hover'], {
              border = 'rounded',
              -- Suppress 'No information available' notification
              silent = false,
            }),
            ['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers['textDocument/signatureHelp'], {
              border = 'rounded',
              -- Suppress 'No information available' notification
              silent = false,
            }),
          },
          flags = {
            debounce_text_changes = 150,
          },
          capabilities = capabilities,
        }

        if on_init then
          lsp['on_init'] = on_init
        end

        if command then
          lsp['cmd'] = command
        end

        lspconfig[server].setup(lsp)
      end
    end,
  },
  {
    'saghen/blink.cmp',
    -- optional: provides snippets for the snippet source
    -- dependencies = { 'rafamadriz/friendly-snippets' },
    version = '1.*',
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      -- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
      -- 'super-tab' for mappings similar to vscode (tab to accept)
      -- 'enter' for enter to accept
      -- 'none' for no mappings
      --
      -- All presets have the following mappings:
      -- C-space: Open menu or open docs if already open
      -- C-n/C-p or Up/Down: Select next/previous item
      -- C-e: Hide menu
      -- C-k: Toggle signature help (if signature.enabled = true)
      --
      -- See :h blink-cmp-config-keymap for defining your own keymap
      keymap = { preset = 'default' },

      appearance = {
        -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        -- Adjusts spacing to ensure icons are aligned
        nerd_font_variant = 'mono',
      },

      -- (Default) Only show the documentation popup when manually triggered
      completion = { documentation = { auto_show = false } },

      -- Default list of enabled providers defined so that you can extend it
      -- elsewhere in your config, without redefining it, due to `opts_extend`
      sources = {
        default = { 'lsp', 'path', 'snippets', 'buffer' },
      },

      -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
      -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
      -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
      --
      -- See the fuzzy documentation for more information
      fuzzy = { implementation = 'prefer_rust_with_warning' },

      cmdline = {
        enabled = true,
        completion = {
          menu = { auto_show = true },
        },
      },
    },
    opts_extend = { 'sources.default' },
  },
}
