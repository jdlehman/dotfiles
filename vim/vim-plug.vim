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
    Plug 'junegunn/gv.vim'
    Plug 'sodapopcan/vim-twiggy'

    " completions
    if has('nvim')
    else
      Plug 'roxma/vim-hug-neovim-rpc'
    end
    Plug 'ncm2/ncm2'
    Plug 'roxma/nvim-yarp'
    Plug 'ncm2/ncm2-bufword'
    Plug 'ncm2/ncm2-path'
    Plug 'ncm2/ncm2-html-subscope'
    Plug 'ncm2/ncm2-markdown-subscope'

    " snippets
    Plug 'SirVer/ultisnips'
    Plug 'ncm2/ncm2-ultisnips'
    Plug 'honza/vim-snippets'

    " language completions
    Plug 'ncm2/ncm2-cssomni'
    Plug 'ncm2/ncm2-tern',  {'do': 'npm install'}
    Plug 'HerringtonDarkholme/yats.vim'
    Plug 'mhartington/nvim-typescript', {'do': './install.sh'}
    Plug 'ncm2/ncm2-jedi'
    Plug 'ncm2/ncm2-racer'
    Plug 'ncm2/ncm2-pyclang'
    Plug 'ncm2/ncm2-vim'
    Plug 'ncm2/ncm2-go'
    Plug 'phpactor/ncm2-phpactor'
    Plug 'ObserverOfTime/ncm2-jc2'

    " Language specific
    if has('nvim')
      Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }
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
    Plug 'StanAngeloff/php.vim', { 'for': 'php' }
    Plug 'iamcco/markdown-preview.nvim', { 'for': 'markdown', 'do': 'cd app & yarn install'  }

    " formatting
    Plug 'editorconfig/editorconfig-vim'
    Plug 'tommcdo/vim-lion'

    " syntax highlighting/styling
    Plug 'w0rp/ale'

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
