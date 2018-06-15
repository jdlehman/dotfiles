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

    " completions
    if has('nvim')
      Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    else
      Plug 'Shougo/deoplete.nvim'
      Plug 'roxma/nvim-yarp'
      Plug 'roxma/vim-hug-neovim-rpc'
    end
    Plug 'mitsuse/autocomplete-swift'
    Plug 'Shougo/neosnippet'
    Plug 'Shougo/neosnippet-snippets'

    " language completions
    Plug 'zchee/deoplete-go', { 'do': 'make'}

    " Language specific
    if has('nvim')
      Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
    end
    Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
    Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
    Plug 'tristen/vim-sparkup', { 'for': ['html', 'eruby'] }
    Plug 'keith/swift.vim', { 'for': 'swift' }
    Plug 'ElmCast/elm-vim', { 'for': 'elm' }
    Plug 'slim-template/vim-slim', { 'for': 'slim' }
    Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
    Plug 'vim-erlang/vim-erlang-runtime', { 'for': 'erlang' }
    Plug 'reasonml-editor/vim-reason-plus', { 'for': 'reason' }
    Plug 'fatih/vim-go', { 'for': 'go', 'do': ':GoUpdateBinaries' }

    " formatting
    Plug 'editorconfig/editorconfig-vim'
    Plug 'tommcdo/vim-lion'

    " syntax highlighting/styling
    Plug 'w0rp/ale'
    Plug 'sbdchd/neoformat'

    " Searching
    Plug 'mileszs/ack.vim'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'

    " Helpful tools
    Plug 'ferranpm/vim-isolate'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-commentary'
    Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
    Plug 'benmills/vimux'
    Plug 'tpope/vim-repeat'
  " }}}
  call plug#end()
" }}}

" MAPPINGS {{{
  " Reinstall/update bundles using vim-plug
  nnoremap <leader>bv :PlugUpgrade<cr>:PlugInstall<cr>:PlugUpdate<cr>:PlugClean<cr>:q<cr>
" }}}
