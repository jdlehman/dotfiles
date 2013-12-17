" Set up vundle
" ==================
filetype off    " Required by vundle

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Bundles
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-surround'
Bundle 'altercation/vim-colors-solarized'
Bundle 'bling/vim-airline'
Bundle 'kien/ctrlp.vim'
Bundle 'rking/ag.vim'
Bundle 'kchmck/vim-coffee-script'

filetype plugin indent on   " Required by vundle

" Bundle specific settings
" ==================

" Use solarized theme
syntax enable
set background=dark
let g:solarized_hitrail=1
colorscheme solarized

" Use vim-airline status bar
let g:airline_powerline_fonts = 1
set laststatus=2

" Use ag for ctrlp
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

" Prevent ag from leaking into terminal
set shellpipe=>

" Set standard vim attributes/settings
" ===================
set mouse=a                           " Allow scrolling and make vim clickable
set hidden                            " Allow buffers to be hidden with unwritten changes
set tabstop=2                         " Tabs are 2 space characters
set shiftwidth=2                      " Indentatons are 2 space characters
set softtabstop=2                     " Tabs in insert mode are 2 space characters
set expandtab                         " Expand tabs to spaces
set autoindent                        " Use indentation from previous line
set smartindent                       " Auto indent based on c-like rules
set backspace=indent,eol,start        " Allow vim to backspace like normal in insert mode
set incsearch                         " Begin searching as soon as text is entered
set hlsearch                          " Highlight search results
set smartcase                         " Case insensitive searches
set number                            " Show line numbers
set nowrap                            " Do not wrap text
set listchars=extends:»,precedes:«    " Chars to display on text off screen
set showmatch                         " Shows matching {,(,if etc. when typing closing },),end
"set showcmd                          " Show command that is being typed
set history=1000                      " Set # of commands to keep in history
set wildignore+=*.swp,*.class,*.o     " Ignore files with these extensions
set backupdir=~/.vim/backup           " Set backup directory
set directory=~/.vim/backup           " Set backup directory
set splitright                        " open vertical splits to the right
set splitbelow                        " open horizontal splits below

" Set key mappings
" =================
" Map leaders
let mapleader=','
let maplocalleader = "\\"

" Quickly open and reload vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr> :nohl <cr>

" Reinstall/update bundles using vundle
nnoremap <leader>bv :BundleInstall<cr>

" Use ag.vim quickly
" ==================
" Search all text in quickfix window
nnoremap <leader>a :Ag!<space>
" Search file names in quickfix window
nnoremap <leader>af :AgFile!<space>
" open quickfix window
nnoremap <leader>ao :copen<cr>
" close quickfix window
nnoremap <leader>ac :ccl<cr>

" Quickly escape insert mode with jj
inoremap jj <ESC>

" Open splits (v vertical, s horizontal)
nnoremap <leader>v <C-w>v
nnoremap <leader>s <C-w>s

" Move around splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Open new tabs
nnoremap <leader>t :tabnew<cr>
" Switch between tabs is gt
" open current split in new tab
nnoremap <leader>nt <C-w>T

" Q=q and W=w in command mode
" Prevents typos when writing or quitting
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Wq wq

" Comment mappings based on file type
augroup comment_group
  autocmd FileType javascript nnoremap <buffer> <localleader>c I//<esc>
  autocmd FileType python     nnoremap <buffer> <localleader>c I#<esc>
  autocmd FileType ruby       nnoremap <buffer> <localleader>c I#<esc>
  autocmd FileType vim        nnoremap <buffer> <localleader>c I"<esc>
augroup END

" Get content inside parenthesis
onoremap p i(

" vimrc graveyard
" =====================

" format html on read and save
" autocmd BufWritePre,BufRead *.html :normal gg=G

" logger
" autocmd VimEnter * -W '~/vimlog2.log'
