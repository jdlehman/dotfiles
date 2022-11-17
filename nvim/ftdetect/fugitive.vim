" http://vimcasts.org/episodes/fugitive-vim-browsing-the-git-object-database/
" auto-clean fugitive buffers
autocmd! BufReadPost fugitive://* set bufhidden=delete
" mapping to view parent tree in fugitive
autocmd! BufReadPost fugitive://*
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> - :edit %:h<cr> |
  \ endif
