call functions#CreateDirectoryIfItDoesNotExists(expand('~') . '/.vim/.tags')
let g:gutentags_cache_dir=expand('~') . '/.vim/.tags'
" let g:gutentags_define_advanced_commands=1

let g:gutentags_exclude_project_root=[
         \'/usr/local/',
         \'phpout'
         \]
let g:gutentags_ctags_exclude=[
         \'*.class',
         \'*.ear',
         \'*.egg-info',
         \'*.errors',
         \'.git',
         \'*.hi',
         \'*.iks',
         \'*.iml',
         \'*.in',
         \'*.jar',
         \'*.log',
         \'*.min.js',
         \'*.o',
         \'*.out',
         \'*.pyc',
         \'*.tar',
         \'*.tar.*',
         \'*.txt',
         \'*.user',
         \'*.war',
         \'*.zip',
         \'.hg',
         \'.idea',
         \'.svn',
         \'Session.vim',
         \'__pycache__',
         \'jQuery-*.*.js',
         \'jQuery.*.js',
         \'phpout',
         \'tags',
         \'*.deb',
         \'*.csr',
         \'*.crt',
         \'*.gpg',
         \'*.changes',
         \'*.db'
         \]
