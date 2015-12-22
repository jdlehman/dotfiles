" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  " performance gain
  " let g:gitgutter_realtime = 0
  " let g:gitgutter_eager = 0
  " appearance
  " highlight SignColumn ctermbg=234
  " highlight GitGutterAdd ctermfg=2 ctermbg=234
  " highlight GitGutterChange ctermfg=3 ctermbg=234
  " highlight GitGutterDelete ctermfg=1 ctermbg=234
" }}}

" MAPPINGS {{{
  " Highlight git gutter change lines
  nnoremap <leader>c :GitGutterLineHighlightsToggle<cr>
" }}}
