" Set up vundle
" ==================
filetype off    " required by vundle

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Bundles
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-surround'
Bundle 'altercation/vim-colors-solarized'
Bundle 'bling/vim-airline'

filetype plugin indent on   " required by vundle

" Bundle specific settings
" ==================

" use solarized theme
syntax enable
set background=dark
let g:solarized_hitrail=1
colorscheme solarized

" use vim-airline status bar
let g:airline_powerline_fonts = 1
set laststatus=2

" set standard vim attributes/settings
" ===================
set mouse=a
set hidden
set ts=2 sts=2 shiftwidth=2 expandtab
set incsearch
set autoindent
set smartindent
set smartcase
set title
set number
set nowrap
set listchars=extends:»,precedes:«
set hlsearch
set showmatch
set showcmd
set history=1000
set wildignore=*.swp,*.class,*.o

" set key mappings
" =================
" map leaders
let mapleader=','
let maplocalleader = "\\"

" quickly open and reload vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr> :nohl <cr>

" Reinstall/update bundles using vundle
nnoremap <leader>bv :BundleInstall<cr>

" quickly escape insert mode with jj
inoremap jj <ESC>

" open splits (v vertical, s horizontal)
" move cursor to new split
nnoremap <leader>v <C-w>v<C-w>l
nnoremap <leader>s <C-w>s<C-w>j

" move around splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Q=q and W=w in command mode
cnoreabbrev W w
cnoreabbrev Q q

" comment mappings based on file type
augroup comment_group
  autocmd FileType javascript nnoremap <buffer> <localleader>c I//<esc>
  autocmd FileType python     nnoremap <buffer> <localleader>c I#<esc>
  autocmd FileType ruby       nnoremap <buffer> <localleader>c I#<esc>
  autocmd FileType vim        nnoremap <buffer> <localleader>c I"<esc>
augroup END

" get content inside parenthesis
onoremap p i(

" vimrc graveyard
" =====================

" format html on read and save
" autocmd BufWritePre,BufRead *.html :normal gg=G

" logger
" autocmd VimEnter * -W '~/vimlog2.log'
