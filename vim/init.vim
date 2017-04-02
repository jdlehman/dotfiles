let $NVIM_TUI_ENABLE_TRUE_COLOR=1

set nocompatible
" LEADERS {{{
  let mapleader=','
  let maplocalleader = "\\"
" }}}

" SET UP vim-plug {{{
  runtime vim-plug.vim
" }}}
filetype plugin indent on

" at top to ensure functions are defined before use
" UTILITY FUNCTIONS {{{
  runtime utilities.vim
" }}}

" ABBREVIATIONS {{{
  runtime abbreviations.vim
" }}}
