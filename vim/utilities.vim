" return list of all open buffers
function! BuffersList()
  let all = range(0, bufnr('$'))
  let list = []
  for buffer in all
    if buflisted(buffer)
      call add(list, bufname(buffer))
    endif
  endfor
  return list
endfunction

" export all vim mappings
function! JL_ExportMappings()
  redir! > vim_keys.txt
    silent verbose map
  redir END
endfunction

" returns vim command output
function! JL_GetCommandOutput(command)
  let save_a = @a
  try
    silent! redir @a
    silent! execute a:command
    redir END
  finally
    " restore register
    let result = @a
    let @a = save_a
    return result
  endtry
endfunction
