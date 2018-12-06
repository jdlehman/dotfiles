" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:neoterm_position = 'horizontal'
" }}}

" MAPPINGS {{{
  nnoremap <silent> ,tf :TREPLSendFile<cr>
  nnoremap <silent> ,tt :TREPLSendLine<cr>
  vnoremap <silent> ,tt :TREPLSendSelection<cr>

  nnoremap <silent> ,tv :Tpos vertical<cr>:Tnew<cr>
  nnoremap <silent> ,ts :Tpos horizontal<cr>:Tnew<cr>
  " hide/close terminal
  nnoremap <silent> ,th :Tclose<cr>
  " clear terminal
  nnoremap <silent> ,tl :call neoterm#clear()<cr>
  " kills the current job (send a <c-c>)
  nnoremap <silent> ,tc :call neoterm#kill()<cr>
" }}}
