command! SaveSession call sessions#SaveSession('mksession', function('vcs#GetGitBranchName'))
command! -bang SaveSession call sessions#SaveSession('mksession!', function('vcs#GetGitBranchName'))
