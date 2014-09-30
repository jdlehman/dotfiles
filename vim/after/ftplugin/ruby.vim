" Run ruby test for current file
nnoremap <leader>mt :call VimuxRunCommand("clear; ruby -I 'lib:test' " . bufname("%"))<cr>
