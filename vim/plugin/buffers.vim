" FUNCTIONS {{{
  " close all hidden buffers
  function! JL_CloseHiddenBuffers()
    " store ids of visible buffers
    let visible_buffers = {}
    for tab_id in range(1, tabpagenr('$'))
      for buffer_id in tabpagebuflist(tab_id)
        let visible_buffers[buffer_id] = 1
      endfor
    endfor

    " close buffers that are not in visible dictionary
    for buffer_id in range(1, bufnr('$'))
      if bufloaded(buffer_id) && !has_key(visible_buffers, buffer_id)
        execute 'bd ' . buffer_id
      endif
    endfor
  endfunction
" }}}

" SETTINGS {{{
" }}}

" MAPPINGS {{{
  " Open splits (v vertical, s horizontal)
  nnoremap <leader>v <c-w>v
  nnoremap <leader>s <c-w>s
  " Move around splits
  nnoremap <c-h> <c-w>h
  nnoremap <c-j> <c-w>j
  nnoremap <c-k> <c-w>k
  nnoremap <c-l> <c-w>l
  " fix issue in terminal updates that cause <c-h> to be <bs>
  nnoremap <bs> <c-w>h

  " Open new tabs
  nnoremap <leader>t :tabnew<cr>
  " Switch between tabs is gt
  " open current split in new tab
  nnoremap <leader>nt <c-w>T

  " see open buffers and select one
  nnoremap <leader>l :ls<cr>:b<space>
  " switch to alternate file
  nnoremap <leader><leader> <c-^>
  " close hidden buffers
  nnoremap <leader>bd :call JL_CloseHiddenBuffers()<cr>
" }}}
