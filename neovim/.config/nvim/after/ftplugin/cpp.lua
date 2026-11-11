local function create_iwyu_commands()
  local iwyu = require('config.cpp.iwyu')
  if not iwyu.available then
    return
  end

  local bufnr = vim.api.nvim_get_current_buf()

  vim.api.nvim_buf_create_user_command(bufnr, 'IWYU', function(options)
    local command = ''

    local scope = options.fargs[1]
    if scope == 'current' then
      command = iwyu.current(vim.api.nvim_buf_get_name(bufnr))
    elseif scope == 'all' then
      command = iwyu.root()
    else
      vim.notify("IWYU: expected 'current' or 'all'", vim.log.levels.ERROR)
      return
    end

    vim.cmd.write()
    vim.cmd('!' .. command)
    vim.cmd.checktime()
  end, {
    desc = 'Run `iwyu` on the current file or all the files in the compilation database',
    nargs = 1,
    complete = function()
      return { 'current', 'all' }
    end,
  })
end

create_iwyu_commands()
