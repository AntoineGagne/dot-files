return {
  cmd = {
    'clangd-20',
    '--clang-tidy',
    '--j=' .. math.max(vim.uv.available_parallelism() - 1, 1),
    '--header-insertion=iwyu',
    '--background-index',
    '--enable-config',
  },
}
