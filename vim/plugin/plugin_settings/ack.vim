" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  " do not display mapping message
  if executable('ag')
    let g:ackprg = 'ag --vimgrep --smart-case'
  endif
" }}}

" MAPPINGS {{{
  " Search all text and add results tp location-list window
  nnoremap <Leader>a :Ack!<Space>
  " append search to existing location-list
  nnoremap <leader>aa :AckAdd<space>
  " Search help files and add results to location-list window
  nnoremap <leader>ah :AgHelp!<space>
" }}}
