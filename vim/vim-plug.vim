" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  " set up vim-plug if not installed
  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !mkdir -p ~/.vim/autoload
    silent !curl -fLo ~/.vim/autoload/plug.vim
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall
  endif

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
    Plug 'ElmCast/elm-vim', { 'for': 'elm' }
    Plug 'slim-template/vim-slim', { 'for': 'slim' }
    Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }

    " syntax highlighting
    Plug 'benekastah/neomake'

    " Searching
    Plug 'rking/ag.vim', { 'on': ['Ag', 'AgAdd', 'AgHelp'] }
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }

    " movement
    Plug 'rhysd/clever-f.vim'
    Plug 'gcmt/wildfire.vim'

    " Helpful tools
    Plug 'ferranpm/vim-isolate'
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
