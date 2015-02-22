" FUNCTIONS {{{
" }}}

" SETTINGS {{{
" }}}

" MAPPINGS {{{
  " move up and down by screen line, not file line
  " makes dealing with line wrapping easier
  nnoremap j gj
  nnoremap k gk
  vnoremap j gj
  vnoremap k gk

  " move to the beginning or end of the line without strecthing
  nnoremap H 0
  nnoremap L $

  " Arrow keys resize current viewport
  nnoremap <left> :vertical resize -5<cr>
  nnoremap <right> :vertical resize +5<cr>
  nnoremap <up> :resize +5<cr>
  nnoremap <Down> :resize -5<cr>

  " move visual blocks of text
  vnoremap <left> <gv^
  vnoremap <right> >gv^
  vnoremap <up> xkP`[V`]
  vnoremap <down> xp`[V`]
" }}}
