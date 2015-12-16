" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:neomake_javascript_enabled_makers = ['eslint']
" }}}

" MAPPINGS {{{
" }}}

" AUTOCOMMANDS {{{
  autocmd! BufWritePost * Neomake
" }}}
