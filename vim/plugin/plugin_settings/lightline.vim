" FUNCTIONS {{{
  function! JLModified()
    return &filetype =~ 'help\|netrw\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
  endfunction

  function! JLReadonly()
    return &filetype !~? 'help\|netrw\|gundo' && &readonly ? 'RO' : ''
  endfunction

  function! JLFilename()
    " use full path or just file name depending on screen width
    let filename = winwidth(0) > 90 ? @% : expand('%:t')
    " do not show filename for help, gundo, fugitive etc.
    if &filetype =~ 'help\|gundo\|^git' || filename =~ '^\w\+:/'
      return ''
    end
    return ('' != JLReadonly() ? JLReadonly() . ' ' : '') .
           \ ('' != filename ? filename : '[No Name]') .
           \ ('' != JLModified() ? ' ' . JLModified() : '')
  endfunction

  function! JLFugitive()
    if expand('%:t') !~? 'Gundo' && exists('*fugitive#head')
      let mark = ''  " edit here for cool mark
      let _ = fugitive#head()
      return strlen(_) ? mark._ : ''
    endif
    return ''
  endfunction

  function! JLFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no filetype') : ''
  endfunction

  function! JLGitGutter()
    let git_data = GitGutterGetHunkSummary()
    let added = git_data[0] > 0 ? git_data[0] . '+ ' : ''
    let modified = git_data[1] > 0 ? git_data[1] . '~ ' : ''
    let deleted = git_data[2] > 0 ? git_data[2] . '-' : ''
    return winwidth(0) > 80 ? (added . modified . deleted) : ''
  endfunction
" }}}

" SETTINGS {{{
  set laststatus=2
  let g:lightline = {
    \ 'active': {
    \   'left': [ ['mode', 'paste'], ['gitgutter', 'fugitive', 'filename'] ],
    \   'right': [ ['lineinfo'], ['percent'], ['filetype'] ]
    \ },
    \ 'component_function': {
    \   'modified': 'JLModified',
    \   'readonly': 'JLReadOnly',
    \   'filename': 'JLFilename',
    \   'filetype': 'JLFiletype',
    \   'fugitive': 'JLFugitive',
    \   'gitgutter': 'JLGitGutter'
    \ },
    \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
    \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" },
  \ }
" }}}

" MAPPINGS {{{
" }}}
