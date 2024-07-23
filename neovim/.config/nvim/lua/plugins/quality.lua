return {
  {
    'mfussenegger/nvim-lint',
    config = function()
      local lint = require('lint')
      lint.linters_by_ft = {
        dockerfile = { 'hadolint' },
        yaml = { 'yamllint' },
        zsh = { 'zsh' }
      }

      local group = vim.api.nvim_create_augroup('linting', { clear = true })
      vim.api.nvim_create_autocmd(
        { 'BufEnter', 'BufWritePost', 'InsertLeave', 'TextChangedI' },
        {
          callback = function()
            lint.try_lint()
          end,
          group = group
        }
      )
    end
  },
  {
    'stevearc/conform.nvim',
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    ---@module "conform"
    ---@type conform.setupOpts
    opts = {
      formatters_by_ft = {
        awk = { 'awk' },
        erlang = { 'erlfmt' },
        elixir = { 'mix' },
        jq = { 'jq' },
        lua = { 'stylua' },
        rust = { 'rustfmt', lsp_format = 'fallback' },
        sh = { 'shellcheck' },
      },
      default_format_opts = {
        lsp_format = "fallback",
      },
      format_on_save = {
        timeout_ms = 500,
      }
    },
    init = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    end
  }
}
