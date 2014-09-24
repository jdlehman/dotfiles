" FUNCTIONS {{{
  " from Gary Bernhardt's vimrc
  function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
      execute ':saveas ' . new_name
      execute ':silent !rm ' . old_name
      redraw!
    endif
  endfunction

  " set working directory to git project root
  " or directory of current file if not git project
  function! JLSetProjectRoot()
    let current_file = expand('%:p')
    " do not mess with 'fugitive://' etc
    if current_file =~ '^\w\+:/' || &filetype =~ '^git'
      return
    endif

    lcd %:p:h
    let git_dir = system("git rev-parse --show-toplevel")
    " See if the command output starts with 'fatal' (if it does, not in a git repo)
    let is_not_git_dir = matchstr(git_dir, '^fatal:.*')
    " if git project, change local directory to git project root
    if (git_dir != "") && empty(is_not_git_dir)
      lcd `=git_dir`
    endif
  endfunction

  " follow symlinked file
  function! JLFollowSymlink()
    let current_file = expand('%:p')
    " do not mess with 'fugitive://' etc
    if current_file =~ '^\w\+:/' || &filetype =~ '^git'
      return
    endif
    if getftype(current_file) == 'link'
      let actual_file = resolve(current_file)
      silent! execute 'file ' . actual_file
    end
  endfunction

  " put cursor on line last edited when buffer was last open
  function! JL_SetCursorPosition()
    if &filetype !~ 'netrw\|^git'
      if line("'\"") > 0 && line("'\"") <= line("$") |
        execute "normal! g`\"" |
      endif
    endif
  endfunction
" }}}

" SETTINGS {{{
" }}}

" MAPPINGS {{{
  " rename current file
  nnoremap <leader>r :call RenameFile()<cr>
" }}}

" AUTOCOMMANDS {{{
  augroup working_directory
    autocmd!
    " follow symlink and set working directory
    autocmd BufRead *
      \ call JLFollowSymlink() |
      \ call JLSetProjectRoot()
    " netrw: follow symlink and set working directory
    autocmd CursorMoved *
      \ if &filetype == 'netrw' |
      \   call JLFollowSymlink() |
      \   call JLSetProjectRoot() |
      \ endif
  augroup END

  augroup file_position
    autocmd!
    " Return to last edit position when opening files
    autocmd BufReadPost * call JL_SetCursorPosition()
  augroup END
" }}}
