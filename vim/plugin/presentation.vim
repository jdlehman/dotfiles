" FUNCTIONS {{{
  " Show syntax highlighting groups for word under cursor
  " via: http://vimcasts.org/episodes/creating-colorschemes-for-vim/
  function! <sid>SynStack()
    if !exists("*synstack")
      return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
  endfunction
" }}}

" SETTINGS {{{
  " highlight trailing whitespaces
  highlight TrailingWhiteSpace ctermbg=red
  match TrailingWhiteSpace /\s\+$/
" }}}

" MAPPINGS {{{
  " toggle word wrap
  nnoremap <leader>w :setlocal wrap!<cr>

  " toggle spell check
  nnoremap <leader>sp :setlocal spell!<cr>

  " Turn off highlight
  nnoremap <leader>h :nohl<cr>

  " Remove trailing spaces
  nnoremap <leader><space> :%s/\s\+$<cr>``

  " Fix indent on code block (paragraph)
  nnoremap <leader>= =ip

  " show Syntax Highlighting group for item under cursor
  " useful for creating color schemes
  nnoremap sh :call <sid>SynStack()<cr>
" }}}

" AUTOCOMMANDS {{{
  augroup trailing_white_space
    autocmd!
    " toggle trailing whitespace on bufwin enter/leave
    autocmd BufWinEnter * match TrailingWhiteSpace /\s\+$/
    autocmd BufWinLeave * call clearmatches()

    " do not show trailing whitespace in insert mode
    autocmd InsertEnter * match TrailingWhiteSpace /\s\+\%#\@<!$/
    " show trailing whitespace
    autocmd InsertLeave * match TrailingWhiteSpace /\s\+$/
  augroup END

  augroup line_numbers
    autocmd!
    " show number when in insert mode show relative number with
    " number on current line outside of insert mode
    autocmd InsertEnter * set number | set norelativenumber
    autocmd InsertLeave * set relativenumber
  augroup END
" }}}
