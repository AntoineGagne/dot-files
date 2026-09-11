local processors = tonumber(vim.fn.system({ 'nproc' }))
local processors_flag = ''
if processors ~= 0 then
  processors_flag = '--j=' .. (processors - 1)
end

return {
  cmd = {
    'clangd-20',
    '--clang-tidy',
    processors_flag,
    '--header-insertion=iwyu',
    '--background-index',
    '--enable-config',
  },
}
