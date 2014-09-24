" FUNCTIONS {{{
  " Qargs command to add quickfix items to argslist
  " then can use argdo <stuff> to run command on all items in argslist
  " argdo update to save all buffers
  " http://stackoverflow.com/a/5686810
  command! -nargs=0 -bar Qargs execute 'args ' . QuickfixFilenames()
  function! QuickfixFilenames()
    " Building a hash ensures we get each buffer only once
    let buffer_numbers = {}
    for quickfix_item in getqflist()
      let buffer_numbers[quickfix_item['bufnr']] = bufname(quickfix_item['bufnr'])
    endfor
    return join(values(buffer_numbers))
  endfunction

  " returns whether or not the quickfix window is open
  function! JL_IsQuickFixOpen()
    return JL_GetCommandOutput("buffers") =~ 'QuickFix'
  endfunction

  " toggle quickfix window
  function! JL_ToggleQuickFix()
    if JL_IsQuickFixOpen()
      cclose
    else
      copen 10
    endif
  endfunction
" }}}

" SETTINGS {{{
" }}}

" MAPPINGS {{{
  " toggle quickfix window
  nnoremap <leader>q :call JL_ToggleQuickFix()<cr>
  " next item in clearfix list
  nnoremap <leader>j :cnext<cr>
  " previous item in clearfix list
  nnoremap <leader>k :cprev<cr>
" }}}
