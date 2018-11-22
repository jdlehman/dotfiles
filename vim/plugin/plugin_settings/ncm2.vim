" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  " enable ncm2 for all buffers
  autocmd BufEnter * call ncm2#enable_for_buffer()

  " IMPORTANTE: :help Ncm2PopupOpen for more information
  set completeopt=noinsert,menuone,noselect

  " suppress the annoying 'match x of y', 'The only match' and 'Pattern not
  " found' messages
  set shortmess+=c

  " CTRL-C doesn't trigger the InsertLeave autocmd . map to <ESC> instead.
  inoremap <c-c> <ESC>

  " suppress the annoying 'match x of y', 'The only match' and 'Pattern not
  " found' messages
  set shortmess+=c

  " CTRL-C doesn't trigger the InsertLeave autocmd . map to <ESC> instead.
  inoremap <c-c> <ESC>
" }}}

" MAPPINGS {{{
" }}}

" AUTOCOMMANDS {{{
" }}}
