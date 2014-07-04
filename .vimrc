set nocompatible
" SET UP VUNDLE {{{
  filetype off    " Required by vundle

call plug#begin('~/.vim/plugged')
  " PLUGINS {{{
    " Style
    Plug 'altercation/vim-colors-solarized'
    Plug 'itchyny/lightline.vim'
    Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }

    " git related
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'

    " Language specific
    Plug 'tpope/vim-rails'
    Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
    Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
    Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
    Plug 'tristen/vim-sparkup', { 'for': ['html', 'eruby'] }

    " Searching
    Plug 'rking/ag.vim', { 'on': 'Ag' }
    Plug 'junegunn/fzf', { 'on': 'FZF' }

    " Helpful tools
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-commentary'
    Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
  " }}}
call plug#end()

filetype plugin indent on   " Required by vundle

" PLUGIN SETTINGS {{{
  " Use solarized theme
  syntax enable
  let g:solarized_hitrail = 1
  set background=dark
  colorscheme solarized

  " Limelight settings
  let g:limelight_conceal_ctermfg = 'gray'
  let g:limelight_conceal_ctermfg = 240

  nnoremap <leader>z :call Test()<cr>
  function! Test()
    let items = fzf#run({ 'options': '-m +c', 'dir': '&pwd', 'source': 'Ag' })
    echo items
  endfunction

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
    \   'modified': 'JLModified',
    \   'readonly': 'JLReadOnly',
    \   'filename': 'JLFilename',
    \   'filetype': 'JLFiletype',
    \   'fugitive': 'JLFugitive',
    \   'gitgutter': 'JLGitGutter'
    \ },
    \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
    \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" },
  \ }

  function! JLModified()
    return &filetype =~ 'help\|netrw\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
  endfunction

  function! JLReadonly()
    return &filetype !~? 'help\|netrw\|gundo' && &readonly ? 'RO' : ''
  endfunction

  function! JLFilename()
    " use full path or just file name depending on screen width
    let filename = winwidth(0) > 90 ? expand('%:p') : expand('%:t')
    return ('' != JLReadonly() ? JLReadonly() . ' ' : '') .
           \ ('' != filename ? filename : '[No Name]') .
           \ ('' != JLModified() ? ' ' . JLModified() : '')
  endfunction

  function! JLFugitive()
    if expand('%:t') !~? 'Gundo' && exists('*fugitive#head')
      let mark = ''  " edit here for cool mark
      let _ = fugitive#head()
      return strlen(_) ? mark._ : ''
    endif
    return ''
  endfunction

  function! JLFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no filetype') : ''
  endfunction

  function! JLGitGutter()
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
" }}}

" netrw settings
" hide help text at top
let g:netrw_banner=0
" use current files directory
let g:netrw_keepdir=0

" Prevent ag from leaking into terminal
set shellpipe=>

" Fix mouse bug in iterm
" Without this, clicking on parts of
" rightmost split does not work correctly
if has('mouse_sgr')
  set ttymouse=sgr
endif

" STANDARD VIM ATTRIBUTES/SETTINGS {{{
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
  set ignorecase smartcase              " Case insensitive searches unless capital letter used
  set number                            " Show line numbers
  set relativenumber                    " Show relative line numbers (in conjuction with number, shows relative numbers and line number on cursor)
  set nowrap                            " Do not wrap text
  set listchars=extends:»,precedes:«    " Chars to display on text off screen
  set showmatch                         " Shows matching {,(,if etc. when typing closing },),end
  set history=1000                      " Set # of commands to keep in history
  set wildignore+=*.swp,*.class,*.o     " Ignore files with these extensions
  set backupdir^=~/.vim/.backup//       " Where to store backup files
  set directory^=~/.vim/.tmp//          " Where to store swap files
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
  set linebreak                         " do not split up words when wrapping
  set encoding=utf-8                    " use utf-8 encoding
  "set showcmd                          " Show command that is being typed
" }}}

" use Ag if available instead of grep
if executable("ag")
  set grepprg=ag\ --nogroup\ --nocolor\ --column\ --smart-case
endif

" persist undos across sessions (github/joelhooks/dotfiles)
if has("persistent_undo")
  set undodir^=~/.vim/.undo//
  set undofile
endif

" KEY MAPPINGS {{{
  " Map leaders
  let mapleader=','
  let maplocalleader = "\\"

  " Quickly open and reload vimrc
  nnoremap <leader>ev :vsplit $MYVIMRC<cr>
  nnoremap <leader>sv :source $MYVIMRC<cr> :nohl <cr>

  " Reinstall/update bundles using vundle
  nnoremap <leader>bv :PlugUpdate<cr>:PlugClean!<cr>:PlugInstall<cr>

  " Toggle between light/dark theme
  call togglebg#map("<leader>bg")

  " use magic regex by default
  " don't have to escape everything anymore
  nnoremap / /\v
  vnoremap / /\v

  " toggle word wrap
  nnoremap <leader>w :set wrap!<cr>

  " move up and down by screen line, not file line
  " makes dealing with line wrapping easier
  nnoremap j gj
  nnoremap k gk
  vnoremap j gj
  vnoremap k gk

  " map - to explore
  nnoremap - :Explore<cr>

  " Copy to system clipboard
  vnoremap <leader>y "*y
  nnoremap <leader>y "*y
  " Paste from the system clipboard using paste mode
  noremap <leader>p :set paste<cr>:put  *<cr>:set nopaste<cr>

  " Go to mark
  nnoremap <leader>g `

  " AG.VIM {{{
    " Search all text in quickfix window
    nnoremap <leader>a :Ag!<space>
    " Search file names in quickfix window
    nnoremap <leader>af :AgFile!<space>
    " open quickfix window
    nnoremap <leader>ao :copen<cr>
    " close quickfix window
    nnoremap <leader>ac :ccl<cr>
  " }}}

  " fzf fuzzy search
  nnoremap <leader>f :FZF<cr>

  " toggle gundo
  nnoremap <leader>u :GundoToggle<cr>

  " Quickly escape insert mode with jk
  inoremap jk <esc>
  " quickly escape visual mode with space
  vnoremap <space> <esc>

  " Map space to colon
  nnoremap <space> :

  " Remove trailing spaces
  nnoremap <leader><space> :%s/\s\+$<cr>

  " Turn off highlight
  nnoremap <leader>h :nohl<cr>

  " Highlight git gutter change lines
  nnoremap <leader>c :GitGutterLineHighlightsToggle<cr>

  " Toggle Limelight highlighting
  nnoremap <leader>l :Limelight!!<cr>

  " Open splits (v vertical, s horizontal)
  nnoremap <leader>v <c-w>v
  nnoremap <leader>s <c-w>s

  " Move around splits
  nnoremap <c-h> <c-w>h
  nnoremap <c-j> <c-w>j
  nnoremap <c-k> <c-w>k
  nnoremap <c-l> <c-w>l

  " Open new tabs
  nnoremap <leader>t :tabnew<cr>
  " Switch between tabs is gt
  " open current split in new tab
  nnoremap <leader>nt <c-w>T

  " Arrow keys resize current viewport
  nnoremap <left> :vertical resize -5<cr>
  nnoremap <right> :vertical resize +5<cr>
  nnoremap <up> :resize +5<cr>
  nnoremap <Down> :resize -5<cr>

  " move visual blocks of text
  vnoremap <left> <gv^
  vnoremap <right> >gv^
  vnoremap <up> xkP`[V`]
  vnoremap <down> xp`[V`]

  " select last text visual selected
  " normal gv does this based on line numbers
  " so is inaccurate if the visual line is moved
  nnoremap gv `[v`]
" }}}

" ABBREVIATIONS {{
  " Q=q and W=w in command mode
  " Prevents typos when writing or quitting
  cnoreabbrev W w
  cnoreabbrev Q q
  cnoreabbrev Wq wq
" }}}

" FUNCTIONS {{{
  " set working directory to git project root
  " or directory of current file if not git project
  function! JLSetProjectRoot()
    let currentFile = expand('%:p')
    " do not mess with 'fugitive://' etc
    if currentFile =~ '^\w\+:/'
      return
    endif

    lcd %:p:h
    let gitdir=system("git rev-parse --show-toplevel")
    " See if the command output starts with 'fatal' (if it does, not in a git repo)
    let isnotgitdir=matchstr(gitdir, '^fatal:.*')
    " if git project, change local directory to git project root
    if empty(isnotgitdir)
      lcd `=gitdir`
    endif
  endfunction

  " follow symlinked file
  function! JLFollowSymlink()
    let currentFile = expand('%:p')
    " do not mess with 'fugitive://' etc
    if currentFile =~ '^\w\+:/'
      return
    endif
    if getftype(currentFile) == 'link'
      let actualFile = resolve(currentFile)
      silent! exec 'file ' . actualFile
    end
  endfunction
  " }}}

" AUTO COMMANDS {{{
  augroup insert_group
    autocmd!
    " show number when in insert mode show relative number with
    " number on current line outside of insert mode
    autocmd InsertEnter * set number | set norelativenumber
    autocmd InsertLeave * set relativenumber
  augroup END

  " File Types
  augroup file_type_group
    autocmd!
    " set json filetype to javascript
    autocmd BufNewFile,BufRead *.json set filetype=javascript
    " set md to markdown file type
    autocmd BufNewFile,BufReadPost *.md set filetype=markdown
  augroup END

  " open buffer
  augroup buf_enter
    autocmd!
    autocmd BufRead *
      \ call JLFollowSymlink() |
      \ call JLSetProjectRoot()
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
" }}}


" vimrc graveyard {{{
  " Make folds persistent
  " autocmd BufWinLeave *.* mkview
  " autocmd BufWinEnter *.* silent loadview
" }}}
