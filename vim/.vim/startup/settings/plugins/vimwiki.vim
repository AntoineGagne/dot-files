let wiki = {}
let wiki.path = '~/vimwiki/'
let wiki.nested_syntaxes = {
            \'c++': 'cpp',
            \'css': 'css',
            \'elm': 'elm',
            \'haskell': 'haskell',
            \'html': 'html',
            \'java': 'java',
            \'javascript': 'javascript',
            \'lua': 'lua',
            \'markdown': 'markdown',
            \'perl': 'perl',
            \'php': 'php',
            \'python': 'python',
            \'ruby': 'ruby',
            \'rust': 'rust',
            \'sh': 'sh',
            \'sql': 'sql',
            \'tex': 'plaintex',
            \'viml': 'vim',
            \'xml': 'xml'
            \}
" let wiki.syntax = 'markdown'
" let wiki.ext = '.md'
let wiki.automatic_nested_syntaxes = 1
let wiki.auto_toc = 1

let g:vimwiki_list = [wiki]
