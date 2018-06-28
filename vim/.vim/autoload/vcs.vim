function! vcs#GetGitBranchName()
    let current_branch = system("git symbolic-ref -q --short HEAD")

    return strings#Chomp(current_branch)
endfunction
