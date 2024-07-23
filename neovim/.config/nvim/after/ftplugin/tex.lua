vim.bo.spell = true
vim.bo.spelllang = 'fr'

-- Enable syntax folding
vim.g.tex_fold_enabled = 1

-- Open file in LaTeX mode by default
vim.g.tex_flavor = 'latex'

-- a = accents/ligatures
-- d = delimiters
-- m = math symbols
-- g = Greek
-- s = superscripts/subscripts
vim.g.tex_conceal = 'adgm'

-- Taken from https://vi.stackexchange.com/a/2360
-- (consulted on Wed Mar  7 16:34:20 EST 2018)
-- vim.cmd([setlocal dictionary+=] . $HOME . "/.vim/dictionaries/" . expand('<amatch>'))
-- setlocal completeopt=menuone,longest,preview
-- setlocal complete+=k
-- setlocal foldmethod=syntax
