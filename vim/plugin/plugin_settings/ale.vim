" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_linter_aliases = {'reason': 'ocaml'}

  let g:ale_fixers = {}
  " JS
  let g:ale_fixers['javascript'] = ['prettier']
  let g:ale_javascript_prettier_options = '--single-quote --parser flow'
  " CSS
  let g:ale_fixers['css'] = ['prettier']
" }}}

" MAPPINGS {{{
" }}}

autocmd! BufWritePre *.js,*.css ALEFix
