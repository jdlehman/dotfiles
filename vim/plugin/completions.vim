" FUNCTIONS {{{
  " Multipurpose tab key via Gary Bernhardt
  " indent if at beginning of line, else completion
  function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
      return "\<tab>"
    else
      return "\<c-p>"
    endif
  endfunction
" }}}

" SETTINGS {{{
" }}}

" MAPPINGS {{{
  " tab indents at beginning of line, otherwise forward completions
  inoremap <tab> <c-r>=InsertTabWrapper()<cr>
  " shift tab goes backwards through completions
  inoremap <s-tab> <c-n>
" }}}
