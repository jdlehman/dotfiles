set nocompatible
" =============
" Set up vundle
" =============
filetype off    " Required by vundle

" set runtime path to include vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" =======
" Bundles
" =======
Plugin 'gmarik/Vundle.vim'

" Style
Plugin 'altercation/vim-colors-solarized'
Plugin 'itchyny/lightline.vim'

" git related
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'

" Language specific
Plugin 'tpope/vim-rails'
Plugin 'kchmck/vim-coffee-script'
Plugin 'pangloss/vim-javascript'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'mattn/emmet-vim'

" Searching
Plugin 'rking/ag.vim'
Plugin 'junegunn/fzf'

" Helpful tools
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'sjl/gundo.vim'

" all plugins must be added before here
call vundle#end()

filetype plugin indent on   " Required by vundle

" ========================
" Bundle specific settings
" ========================

" Use solarized theme
syntax enable
let g:solarized_hitrail = 1
set background=dark
colorscheme solarized

" *********************
" Begin lightline setup
" *********************
set laststatus=2
let g:lightline = {
  \ 'active': {
  \   'left': [ ['mode', 'paste'], ['gitgutter', 'fugitive', 'filename'] ],
  \   'right': [ ['lineinfo'], ['percent'], ['filetype'] ]
  \ },
  \ 'component_function': {
  \   'modified': 'MyModified',
  \   'readonly': 'MyReadOnly',
  \   'filename': 'MyFilename',
  \   'filetype': 'MyFiletype',
  \   'fugitive': 'MyFugitive',
  \   'gitgutter': 'MyGitGutter'
  \ },
  \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
  \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" },
\ }

function! MyModified()
  return &filetype =~ 'help\|netrw\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! MyReadonly()
  return &filetype !~? 'help\|netrw\|gundo' && &readonly ? 'RO' : ''
endfunction

function! MyFilename()
  " use full path or just file name depending on screen width
  let filename = winwidth(0) > 90 ? expand('%:p') : expand('%:t')
  return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
         \ ('' != filename ? filename : '[No Name]') .
         \ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction

function! MyFugitive()
  if expand('%:t') !~? 'Gundo' && exists('*fugitive#head')
    let mark = ''  " edit here for cool mark
    let _ = fugitive#head()
    return strlen(_) ? mark._ : ''
  endif
  return ''
endfunction

function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no filetype') : ''
endfunction

function! MyGitGutter()
  let gitdata = GitGutterGetHunkSummary()
  let added = gitdata[0] > 0 ? gitdata[0] . '+ ' : ''
  let modified = gitdata[1] > 0 ? gitdata[1] . '~ ' : ''
  let deleted = gitdata[2] > 0 ? gitdata[2] . '-' : ''
  return winwidth(0) > 80 ? (added . modified . deleted) : ''
endfunction
" *******************
" End lightline setup
" *******************

" gitgutter settings
" performance gain
let g:gitgutter_realtime = 0
let g:gitgutter_eager = 0
" appearance
highlight SignColumn ctermbg=234
highlight GitGutterAdd ctermfg=2 ctermbg=234
highlight GitGutterChange ctermfg=3 ctermbg=234
highlight GitGutterDelete ctermfg=1 ctermbg=234

" netrw settings
" hide help text at top
let g:netrw_banner=0
" use current files directory
let g:netrw_keepdir= 0

" Prevent ag from leaking into terminal
set shellpipe=>

" Fix mouse bug in iterm
" Without this, clicking on parts of
" rightmost split does not work correctly
if has('mouse_sgr')
  set ttymouse=sgr
endif

" ====================================
" Set standard vim attributes/settings
" ====================================
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
set relativenumber                    " Show relative line numbers (in conjuction with number, shows relative numbers and line number on cursor)
set nowrap                            " Do not wrap text
set listchars=extends:»,precedes:«    " Chars to display on text off screen
set showmatch                         " Shows matching {,(,if etc. when typing closing },),end
set history=1000                      " Set # of commands to keep in history
set wildignore+=*.swp,*.class,*.o     " Ignore files with these extensions
set backupdir=~/.vim/backup           " Set backup directory
set directory=~/.vim/backup           " Set backup directory
set splitright                        " open vertical splits to the right
set splitbelow                        " open horizontal splits below
set timeoutlen=300                    " Set key stroke timeout
set ttimeoutlen=10                    " Set key stroke timeout
set wildmenu                          " enable bash style tab completion
set wildmode=list:longest,full        " enable bash style tab completion
set lazyredraw                        " only redraw when necessary
set cursorline                        " highlight current line
set autoread                          " reload files changed outside of vim
set viminfo^=%                        " Remember info about open buffers on close
set nofoldenable                      " disable folding
set linebreak                         " do not split up words when wrapping
set autochdir                         " automatically change working directory to directory of current file
"set showcmd                          " Show command that is being typed

" persist undos across sessions (github/joelhooks/dotfiles)
if has("persistent_undo")
  set undodir=~/.vim/undodir
  set undofile
endif

" ================
" Set key mappings
" ================
" Map leaders
let mapleader=','
let maplocalleader = "\\"

" Quickly open and reload vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr> :nohl <cr>

" Reinstall/update bundles using vundle
nnoremap <leader>bv :PluginClean<cr>y:PluginInstall<cr>

" Toggle between light/dark theme
call togglebg#map("<leader>b")

" toggle word wrap
nnoremap <leader>w :set wrap!<cr>

" map - to explore
nnoremap - :Explore<cr>

" Copy to system clipboard
vnoremap <leader>y "*y
nnoremap <leader>y "*y
" Paste from the system clipboard using paste mode
noremap <leader>p :set paste<CR>:put  *<CR>:set nopaste<CR>

" Go to mark
nnoremap <leader>g `

" ==================
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

" fzf fuzzy search
nnoremap <leader>f :FZF<cr>

" toggle gundo
nnoremap <leader>u :GundoToggle<CR>

" Quickly escape insert mode with jk
inoremap jk <ESC>
" quickly escape visual mode with space
vnoremap <space> <ESC>

" Map space to colon
nnoremap <space> :

" Remove trailing spaces
nnoremap <leader><space> :%s/\s\+$<cr>

" Turn off highlight
nnoremap <leader>h :nohl<cr>

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

" Arrow keys resize current viewport
nnoremap <Left> :vertical resize -5<CR>
nnoremap <Right> :vertical resize +5<CR>
nnoremap <Up> :resize +5<CR>
nnoremap <Down> :resize -5<CR>

" move visual blocks of text
vnoremap <Left> <gv^
vnoremap <Right> >gv^
vnoremap <Up> xkP`[V`]
vnoremap <Down> xp`[V`]

" select last text visual selected
" normal gv does this based on line numbers
" so is inaccurate if the visual line is moved
nnoremap gv `[v`]

" Q=q and W=w in command mode
" Prevents typos when writing or quitting
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Wq wq

" =============
" Auto commands
" =============
augroup insert_group
  autocmd!
  " show number when in insert mode show relative number with
  " number on current line outside of insert mode
  autocmd InsertEnter * :set number | :set norelativenumber
  autocmd InsertLeave * :set relativenumber
augroup END

" File Types
augroup file_type_group
  autocmd!
  " set json filetype to javascript
  autocmd BufNewFile,BufRead *.json set filetype=javascript
  " set md to markdown file type
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown
augroup END

" Autocommands that do not fit anywhere else
augroup wildcard_group
  autocmd!
  " Return to last edit position when opening files (github/joelhooks/dotfiles)
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
augroup END


" ===============
" vimrc graveyard
" ===============
" Make folds persistent
" autocmd BufWinLeave *.* mkview
" autocmd BufWinEnter *.* silent loadview
