" FUNCTIONS {{{
  " fuzzy serach buffers
  command! FZFBuffers call fzf#run({
    \ 'source':  BuffersList(),
    \ 'sink':    'e ',
    \ 'tmux_height': '30%'
  \})

  " fuzzy search most recently opened files
  command! FZFMru call fzf#run({
    \'source': v:oldfiles,
    \'sink' : 'e ',
    \ 'tmux_height': '30%'
  \})
" }}}

" SETTINGS {{{
" }}}

" MAPPINGS {{{
  nnoremap <leader>f :FZF<cr>
  nnoremap <leader>fb :FZFBuffers<cr>
  nnoremap <leader>fm :FZFMru<cr>
" }}}
