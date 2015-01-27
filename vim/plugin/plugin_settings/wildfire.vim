" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:wildfire_objects = {
    \ "*" : ["i'", 'i"', "i)", "i]", "i}", "ip", "it"],
    \ "html,xml" : ["at", "it"],
  \ }
" }}}

" MAPPINGS {{{
  " This selects the next closest text object.
  " nnoremap <cr> <Plug>(wildfire-fuel)

  " This selects the previous closest text object.
  " vnoremap <del> <Plug>(wildfire-water)

  nmap <leader>ws <Plug>(wildfire-quick-select)
" }}}
