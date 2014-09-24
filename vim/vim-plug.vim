" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  call plug#begin('~/.vim/plugged')
  " PLUGINS {{{
    " Style
    Plug 'junegunn/seoul256.vim'
    Plug 'itchyny/lightline.vim'

    " git related
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'

    " Language specific
    Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
    Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
    Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
    Plug 'tristen/vim-sparkup', { 'for': ['html', 'eruby'] }
    Plug 'mustache/vim-mustache-handlebars', { 'for': 'html.handlebars' }

    " Searching
    Plug 'rking/ag.vim', { 'on': ['Ag', 'AgAdd', 'AgHelp'] }
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }

    " Helpful tools
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-commentary'
    Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
    Plug 'benmills/vimux'
  " }}}
  call plug#end()
" }}}

" MAPPINGS {{{
  " Reinstall/update bundles using vim-plug
  nnoremap <leader>bv :PlugUpdate<cr>:PlugClean!<cr>:PlugInstall<cr>:q<cr>
" }}}
