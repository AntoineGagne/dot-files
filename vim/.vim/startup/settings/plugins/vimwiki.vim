let wiki = {}

let wiki.path = '~/vimwiki/'
if executable('pandoc')
    let wiki.custom_wiki2html = expand('~/.vim/vimwiki/pandoc-convert.bash')
    let wiki.template_path = '~/.vim/vimwiki/templates/pandoc/html/'
    let wiki.template_default = 'notes'
    let wiki.template_ext = '.html'
    let wiki.css_name = 'style.css'
endif

let wiki.nested_syntaxes = {
            \'c++': 'cpp',
            \'css': 'css',
            \'elm': 'elm',
            \'haskell': 'haskell',
            \'html': 'html',
            \'java': 'java',
            \'javascript': 'javascript',
            \'json': 'json',
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
            \'xml': 'xml',
            \'erlang': 'erlang'
            \}
" let wiki.syntax = 'markdown'
" let wiki.ext = '.md'
let wiki.automatic_nested_syntaxes = 1
let wiki.auto_toc = 1

let g:vimwiki_list = [wiki]
