" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  let g:neoformat_try_formatprg = 1
" }}}

" MAPPINGS {{{
" }}}

autocmd BufWritePre *.js Neoformat
autocmd FileType javascript set formatprg=prettier\ --stdin\ --single-quote
