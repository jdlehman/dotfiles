" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:go_fmt_command = "goimports"
  let g:go_highlight_build_constraints = 1
  let g:go_highlight_extra_types = 1
  let g:go_highlight_fields = 1
  let g:go_highlight_functions = 1
  let g:go_highlight_methods = 1
  let g:go_highlight_operators = 1
  let g:go_highlight_structs = 1
  let g:go_highlight_types = 1

  let g:go_auto_type_info = 1

  " highlight same ids when hovering
  " let g:go_auto_sameids = 1
" }}}

" MAPPINGS {{{
  au FileType go nmap <leader>gt :GoDeclsDir<cr>
  au Filetype go nmap <leader>ga <Plug>(go-alternate-edit)
  au Filetype go nmap <leader>gas <Plug>(go-alternate-split)
  au Filetype go nmap <leader>gav <Plug>(go-alternate-vertical)
" }}}
