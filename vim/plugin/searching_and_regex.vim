" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  if exists('&inccommand')
    set inccommand=split
  endif
" }}}

" MAPPINGS {{{
  " use magic regex by default
  " don't have to escape everything anymore
  nnoremap / /\v
  vnoremap / /\v
" }}}

