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

" Let YouCompleteMe fetch identifiers ctags files
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_autoclose_preview_window_after_completion=1

" Set the omnicompleter
set omnifunc=syntaxcomplete#Complete
