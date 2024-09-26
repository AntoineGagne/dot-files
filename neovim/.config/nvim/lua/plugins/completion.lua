return {
  { 'honza/vim-snippets' },
  {
    'dcampos/nvim-snippy',
    opts = {
      mappings = {
        is = {
          ['<Tab>'] = 'expand_or_advance',
          ['<S-Tab>'] = 'previous',
        },
        nx = {
          ['<leader>x'] = 'cut_text',
        },
      },
    },
  },
  { 'neovim/nvim-lspconfig' },
  { 'hrsh7th/cmp-nvim-lsp' },
  { 'hrsh7th/cmp-buffer' },
  { 'hrsh7th/cmp-path' },
  { 'hrsh7th/cmp-cmdline' },
  { 'dcampos/cmp-snippy' },
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      'cmp-buffer',
      'cmp-cmdline',
      'cmp-nvim-lsp',
      'cmp-path',
    },
    config = function()
      local cmp = require('cmp')
      cmp.setup({
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
            require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
            vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
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
          { name = 'snippy' },
          { name = 'buffer' },
          { name = 'path' },
        }),
      })

      -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' },
          { name = 'path' },
        },
      })

      cmp.setup.cmdline({ ':' }, {
        autocomplete = false,
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'cmdline' },
        },
      })

      local local_config = require('lsp.config')
      local capabilities = require('cmp_nvim_lsp').default_capabilities()
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
}
