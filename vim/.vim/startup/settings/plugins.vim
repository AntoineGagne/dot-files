" {{{1 Autocompleters

" {{{2 YouCompleteMe
let g:ycm_auto_trigger = 1

" Default 1
let g:ycm_register_as_syntastic_checker = 1
" Default 1
let g:Show_diagnostics_ui = 1

" Enable neco-ghc completion from YouCompleteMe
let g:ycm_semantic_triggers = {
            \ 'haskell' : ['.'],
            \ 'elm' : ['.']
            \}

" Will put icons in Vim's gutter on lines that have a diagnostic set.
" Turning this off will also turn off the YcmErrorLine and YcmWarningLine
" highlighting
let g:ycm_enable_diagnostic_signs = 1
let g:ycm_enable_diagnostic_highlighting = 0
" Default 0
let g:ycm_always_populate_location_list = 1
" Default 1
let g:ycm_open_loclist_on_ycm_diags = 1

" Default 1
let g:ycm_complete_in_strings = 1
" Default 0
let g:ycm_collect_identifiers_from_tags_files = 0
" Default ''
let g:ycm_path_to_python_interpreter = '' 

" Default 0 (logging to console)
let g:ycm_server_use_vim_stdout = 0
" Default info
let g:ycm_server_log_level = 'info'

" Where to search for .ycm_extra_conf.py if not found
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 1

" [ 'same-buffer', 'horizontal-split', 'vertical-split', 'new-tab' ]
let g:ycm_goto_buffer_command = 'same-buffer' 
let g:ycm_filetype_whitelist = { '*': 1 }
let g:ycm_filetype_blacklist = {
            \ 'tex' : 1
            \}
let g:ycm_key_invoke_completion = '<C-Space>'

" Set the omnicompleter
set omnifunc=syntaxcomplete#Complete


" {{{1 Languages

" {{{2 Rust

" {{{3 Racer
let g:racer_cmd = "~/.vim/bundle/racer/target/release/racer"
let $RUST_SRC_PATH="~/rustc-1.5.0/src/"

" {{{2 Haskell

" {{{3 neco-ghc
set path="~/.local/bin/ghc-mod"
let g:necoghc_enable_detailed_browse = 1

" {{{3 haskell-vim
" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
if exists("g:loaded_haskellvim_haskell")
    " To enable highlighting of forall
    let g:haskell_enable_quantification = 1 
    " To enable highlighting of mdo and rec
    let g:haskell_enable_recursivedo = 1
    " To enable highlighting of proc
    let g:haskell_enable_arrowsyntax = 1 
    " To enable highlighting of pattern
    let g:haskell_enable_pattern_synonyms = 1 
    " To enable highlighting of type roles
    let g:haskell_enable_typeroles = 1 
    " To enable highlighting of static
    let g:haskell_enable_static_pointers = 1 

    " Indentation
    let g:haskell_indent_if = 4
    let g:haskell_indent_case = 4
    let g:haskell_indent_let = 4
    let g:haskell_indent_guard = 4
    let g:haskell_indent_where = 4
    let g:haskell_indent_do = 4
    let g:haskell_indent_in = 4
    let g:haskell_classic_highlighting = 0
endif

" {{{3 ghcmod-vim
hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = 'ghcmodType'

" {{{3 Tabular
let g:haskell_tabular = 1

" {{{2 Elm
let g:elm_jump_to_error = 0
let g:elm_make_output_file = "elm.js"
let g:elm_make_show_warnings = 0
let g:elm_syntastic_show_warnings = 0
let g:elm_browser_command = ""
let g:elm_detailed_complete = 0
let g:elm_format_autosave = 0
let g:elm_setup_keybindings = 1
let g:elm_classic_highlighting = 0


" {{{1 UI

" {{{2 vim-airline
let g:airline_theme='distinguished'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#ycm#enabled = 1
let g:airline#extensions#ycm#error_symbol = 'E:'
let g:airline#extensions#ycm#warning_symbol = 'W:'
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#branch#enabled=1
let g:airline#extensions#hunks#enabled=0
let g:airline_detect_spell=1

" {{{2 Colorschemes

" {{{3 Gruvbox
colorscheme gruvbox
set background=dark
let g:gruvbox_italic=1
let g:gruvbox_invert_indent_guides=1


" {{{1 Linting

" {{{2 Syntastic
let g:syntastic_always_populate_loc_list = 1
" Always show the errors list
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0


" {{{1 Formatting

" {{{2 Markdown Table
let g:table_mode_corner_corner="+"
let g:table_mode_header_fillchar="="
