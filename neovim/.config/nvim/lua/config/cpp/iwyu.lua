local function find(candidates)
  for _, name in ipairs(candidates) do
    if vim.fn.executable(name) == 1 then
      return name
    end
  end
end

local tool = find({ 'iwyu_tool', 'iwyu_tool.py', 'iwyu-tool' })
local fixer = find({ 'fix_include', 'iwyu-fix-includes' })

local M = { available = tool ~= nil and fixer ~= nil }
local processors = vim.uv.available_parallelism()
local jobs = ' -j ' .. (processors - 1)

function M.current(file)
  return ('%s -p build %s %s | %s'):format(tool, jobs, vim.fn.shellescape(file), fixer)
end

function M.root()
  return ('%s -p build %s | %s'):format(tool, jobs, fixer)
end

return M
