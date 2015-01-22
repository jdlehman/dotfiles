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

  function! FZFGit()
    " Remove trailing new line to make it work with tmux splits
    let directory = substitute(system('git rev-parse --show-toplevel'), '\n$', '', '')
    if !v:shell_error
      call fzf#run({'sink': 'e', 'dir': directory, 'source': 'git ls-files', 'tmux_height': '40%'})
    else
      FZF
    endif
  endfunction
  command! FZFGit call FZFGit()
" }}}


" SETTINGS {{{
" }}}

" MAPPINGS {{{
  nnoremap <leader>f :FZF<cr>
  nnoremap <leader>fg :FZFGit<cr>
  nnoremap <leader>fb :FZFBuffers<cr>
  nnoremap <leader>fm :FZFMru<cr>
" }}}
