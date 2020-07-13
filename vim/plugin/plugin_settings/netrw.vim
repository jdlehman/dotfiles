" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  " hide help text at top
  let g:netrw_banner=0
  " keep the current directory the same as browsing directory
  " let g:netrw_keepdir=0
" }}}

" MAPPINGS {{{
  " map - to explore
  nmap <expr> - &ft ==# 'netrw' ? "\<Plug>NetrwBrowseUpDir" : ":Explore<cr>"
  nmap <expr> <c-h> &ft ==# 'netrw' ? "<c-w>h" : "<c-w>h"
  nmap <expr> <c-j> &ft ==# 'netrw' ? "<c-w>j" : "<c-w>j"
  nmap <expr> <c-k> &ft ==# 'netrw' ? "<c-w>k" : "<c-w>k"
  nmap <expr> <c-l> &ft ==# 'netrw' ? "<c-w>l" : "<c-w>l"
" }}}

