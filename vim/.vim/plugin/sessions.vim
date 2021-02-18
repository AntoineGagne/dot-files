" {{{1 sessions-vim.vim
let g:session_extension = '.vim'
let g:sessions_directory = expand('~/.vim/.sessions')
let g:session_default_name = 'Session'

command! SaveSession call sessions#SaveSession('mksession', function('vcs#GetGitBranchName'))
command! -bang SaveSession call sessions#SaveSession('mksession!', function('vcs#GetGitBranchName'))
