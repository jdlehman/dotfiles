" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_reason_ols_use_global = 1

  let g:ale_fixers = {}
  " JS
  let g:ale_fixers['javascript'] = ['prettier']
  let g:ale_javascript_prettier_options = '--parser flow'
  " json
  let g:ale_fixers['json'] = ['prettier']
  let g:ale_json_prettier_options = '--parser json --single-quotes'
  " CSS
  let g:ale_fixers['css'] = ['prettier']
  " reason
  let g:ale_fixers['reason'] = ['refmt']
" }}}

" MAPPINGS {{{
" }}}

" autoclose error list when empty
autocmd! User ALELintPost if empty(getloclist(0)) | lclose | endif
autocmd! BufWritePre *.js,*.css,*.json ALEFix
autocmd! BufWritePre *.re,*.rei,*.js,*.css,*.json ALEFix
