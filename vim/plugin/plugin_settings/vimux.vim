" FUNCTIONS {{{
   " send text to another tmux pane
   function! VimuxSlime(...)
     " select paragraph if not already in visual mode
     if a:0 > 0
       normal! gv
     else
       normal! vip
     endif
     " backup register a
     let save_a = @a
     " temporarily store selection to register a
     normal! "ay
     call VimuxSendText(@a)
     " send Enter key unless text ends in newline
     if @a !~ '\n$'
       call VimuxSendKeys("Enter")
     end
     "restore register
     let @a = save_a
   endfunction
" }}}

" SETTINGS {{{
" }}}

" MAPPINGS {{{
   " Prompt for a command to run
   nnoremap <leader>mp :VimuxPromptCommand<cr>

   " Run last command executed by VimuxRunCommand
   nnoremap <leader>ml :VimuxRunLastCommand<cr>

   " Inspect runner pane (focus on runner pane with cursor)
   nnoremap <leader>mi :VimuxInspectRunner<cr>

   " Close vim tmux runner opened by VimuxRunCommand
   nnoremap <leader>mq :VimuxCloseRunner<cr>

   " Interrupt any command running in the runner pane
   nnoremap <leader>mx :VimuxInterruptRunner<cr>

   " Zoom the runner pane (vim pane is still there, just minified)
   nnoremap <leader>mz :VimuxZoomRunner<cr>

   " Send enter key to runner pane
   nnoremap <leader>me :call VimuxSendKeys("Enter")<cr>

   " Send kill signal to runner pane
   nnoremap <leader>mc :call VimuxSendKeys("C-c")<cr>

   " If text is selected, save it in the v buffer and send that buffer it to tmux
   vnoremap <leader>ms :call VimuxSlime(1)<cr>

   " Select current paragraph and send it to tmux
   nnoremap <leader>ms :call VimuxSlime()<cr>
" }}}
