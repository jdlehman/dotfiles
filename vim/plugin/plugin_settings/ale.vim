" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_fix_on_save = 1
  let g:ale_reason_ols_use_global = 1
  let g:ale_javascript_prettier_use_local_config = 1
" }}}

" MAPPINGS {{{
" }}}

" autoclose error list when empty
autocmd! User ALELintPost if empty(getloclist(0)) | lclose | endif
