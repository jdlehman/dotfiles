" Run ruby test for current file
nnoremap <localleader>mt :call VimuxRunCommand("clear; ruby -I 'lib:test' " . bufname("%"))<cr>
