" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  " enable ncm2 for all buffers
  autocmd BufEnter * call ncm2#enable_for_buffer()

  " suppress the annoying 'match x of y', 'The only match' and 'Pattern not
  " found' messages
  set shortmess+=c
" }}}

" MAPPINGS {{{
" }}}

" AUTOCOMMANDS {{{
  au User Ncm2PopupOpen set completeopt=noinsert,menuone,noselect
  au User Ncm2PopupClose set completeopt=menuone
" }}}
