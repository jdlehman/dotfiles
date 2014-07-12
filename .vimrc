set nocompatible
" SET UP vim-plug {{{
  call plug#begin('~/.vim/plugged')
  " PLUGINS {{{
    " Style
    Plug 'altercation/vim-colors-solarized'
    Plug 'itchyny/lightline.vim'

    " git related
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'

    " Language specific
    Plug 'tpope/vim-rails'
    Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
    Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
    Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
    Plug 'tristen/vim-sparkup', { 'for': ['html', 'eruby'] }
    Plug 'mustache/vim-mustache-handlebars', { 'for': 'html' }

    " Searching
    Plug 'rking/ag.vim', { 'on': ['Ag', 'AgAdd', 'AgHelp'] }
    Plug 'junegunn/fzf', { 'on': 'FZF' }

    " Helpful tools
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-commentary'
    Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
  " }}}
  call plug#end()

  filetype plugin indent on
" }}}

" PLUGIN SETTINGS {{{
  " SOLARIZED {{{
    syntax enable
    set background=dark
    colorscheme solarized
  " }}}

  " LIGHTLINE {{{
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
  " }}}

  " GITGUTTER {{{
    " performance gain
    let g:gitgutter_realtime = 0
    let g:gitgutter_eager = 0
    " appearance
    highlight SignColumn ctermbg=234
    highlight GitGutterAdd ctermfg=2 ctermbg=234
    highlight GitGutterChange ctermfg=3 ctermbg=234
    highlight GitGutterDelete ctermfg=1 ctermbg=234
  " }}}

  " MUSTACHE-HANDLEBARS {{{
    let g:mustache_abbreviations = 1
  " }}}

  " AG VIM {{{
    " do not display mapping message
    let g:ag_mapping_message=0
  " }}}
" }}}

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
  set path=**                           " set path to ** off current directory
  set display+=lastline                 " display as much of lastline as possible
  set scrolloff=5                       " minimum rows to keep above/below cursor
  set sidescrolloff=5                   " minimum cols to keep left/right of cursor
  set hidden                            " allow unsaved buffers to be hidden
  set showcmd                           " Show command that is being typed
  " set colorcolumn=80                  " set highlighted column

  " NETRW {{{
    " hide help text at top
    let g:netrw_banner=0
  " }}}

  " Prevent ag from leaking into terminal
  set shellpipe=>

  " Normally, Vim messes with iskeyword when you open a shell file. This can
  " leak out, polluting other file types even after a 'set ft=' change. This
  " variable prevents the iskeyword change so it can't hurt anyone.
  " Via Gary Bernhardt
  let g:sh_noisk=1

  " Fix mouse bug in iterm
  " Without this, clicking on parts of
  " rightmost split does not work correctly
  if has('mouse_sgr')
    set ttymouse=sgr
  endif

  " use Ag if available instead of grep
  if executable("ag")
    set grepprg=ag\ --nogroup\ --nocolor\ --column\ --smart-case
  endif

  " persist undos across sessions (github/joelhooks/dotfiles)
  if has("persistent_undo")
    set undodir^=~/.vim/.undo//
    set undofile
  endif

  " highlight trailing whitespaces
  highlight TrailingWhiteSpace ctermbg=red
  match TrailingWhiteSpace /\s\+$/
" }}}

" KEY MAPPINGS {{{
  " LEADERS {{{
    let mapleader=','
    let maplocalleader = "\\"
  " }}}

  " VIMRC {{{
    " Quickly open and reload vimrc
    nnoremap <leader>ev :vsplit $MYVIMRC<cr>
    nnoremap <leader>sv :source $MYVIMRC<cr> :nohl <cr>

    " Reinstall/update bundles using vim-plug
    nnoremap <leader>bv :PlugUpdate<cr>:PlugClean!<cr>:PlugInstall<cr>
  " }}}

  " PLUGINS {{{
    " SOLARIZED {{{
      " Toggle between light/dark theme
      call togglebg#map("<leader>bg")
    " }}}

    " AG.VIM {{{
      " Search all text and add results tp location-list window
      nnoremap <leader>a :Ag!<space>
      " append search to existing location-list
      nnoremap <leader>aa :AgAdd<space>
      " Search help files and add results to location-list window
      nnoremap <leader>ah :AgHelp!<space>
      " open quickfix window
      nnoremap <leader>ao :copen<cr>
      " close quickfix window
      nnoremap <leader>ac :ccl<cr>
      " next item in clearfix list
      nnoremap <leader>j :cnext<cr>
      " previous item in clearfix list
      nnoremap <leader>k :cprev<cr>
    " }}}

    " FZF {{{
      nnoremap <leader>f :FZF<cr>
    " }}}

    " GUNDO {{{
      " toggle gundo
      nnoremap <leader>u :GundoToggle<cr>
    " }}}

    " GITGUTTER {{{
      " Highlight git gutter change lines
      nnoremap <leader>c :GitGutterLineHighlightsToggle<cr>
    " }}}
  " }}}

  " MOVEMENT {{{
    " move up and down by screen line, not file line
    " makes dealing with line wrapping easier
    nnoremap j gj
    nnoremap k gk
    vnoremap j gj
    vnoremap k gk

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
  " }}}

  " SEARCHING/REGEX {{{
    " use magic regex by default
    " don't have to escape everything anymore
    nnoremap / /\v
    vnoremap / /\v
  " }}}

  " SPLITS/TABS/BUFFERS {{{
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

    " see open buffers and select one
    nnoremap <leader>l :ls<cr>:b<space>
    " switch to alternate file
    nnoremap <leader><leader> <c-^>
  " }}}

  " COPY/PASTING {{{
    " Copy to system clipboard
    vnoremap <leader>y "*y
    nnoremap <leader>y "*y
    " Paste from the system clipboard using paste mode
    noremap <leader>p :set paste<cr>:put  *<cr>:set nopaste<cr>
  " }}}

  " FILE EXPLORATION {{{
    " map - to explore
    nnoremap - :Explore<cr>
  " }}}

  " PRESENTATION UTILITIES {{{
    " toggle word wrap
    nnoremap <leader>w :setlocal wrap!<cr>

    " Turn off highlight
    nnoremap <leader>h :nohl<cr>

    " Remove trailing spaces
    nnoremap <leader><space> :%s/\s\+$<cr>``
  " }}}

  " MODE CHANGING {{{
    " Quickly escape insert mode with jk
    inoremap jk <esc>
    " quickly escape visual mode with space
    vnoremap <space> <esc>
  " }}}

  " MISC {{{
    " Map space to colon
    nnoremap <space> :

    " select last text visual selected
    " normal gv does this based on line numbers
    " so is inaccurate if the visual line is moved
    nnoremap gv `[v`]

    " show syntax Highlighting group for item under cursor
    " useful for creating color schemes
    nnoremap <c-s-h> :call <sid>SynStack()<cr>

    " tab indents at beginning of line, otherwise forward completions
    inoremap <tab> <c-r>=InsertTabWrapper()<cr>
    " shift tab goes backwards through completions
    inoremap <s-tab> <c-n>
  " }}}
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

  " Show syntax highlighting groups for word under cursor
  " via: http://vimcasts.org/episodes/creating-colorschemes-for-vim/
  function! <sid>SynStack()
    if !exists("*synstack")
      return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
  endfunction

  " Multipurpose tab key via Gary Bernhardt
  " indent if at beginning of line, else completion
  function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
      return "\<tab>"
    else
      return "\<c-p>"
    endif
  endfunction
" }}}

" AUTO COMMANDS {{{
  augroup insert_group
    autocmd!
    " show number when in insert mode show relative number with
    " number on current line outside of insert mode
    autocmd InsertEnter * set number | set norelativenumber
    autocmd InsertLeave * set relativenumber

    " do not show trailing whitespace in insert mode
    autocmd InsertEnter * match TrailingWhiteSpace /\s\+\%#\@<!$/
    " show trailing whitespace
    autocmd InsertLeave * match TrailingWhiteSpace /\s\+$/
  augroup END

  " File Types
  augroup file_type_group
    autocmd!
    " set json filetype to javascript
    autocmd BufNewFile,BufRead *.json set filetype=javascript
    " set md to markdown file type
    autocmd BufNewFile,BufReadPost *.md set filetype=markdown
    " wrap on markdown files
    autocmd Filetype markdown setlocal wrap
  augroup END

  " buffer events
  augroup buffer_events
    autocmd!
    " follow symlink and set working directory
    autocmd BufRead *
      \ call JLFollowSymlink() |
      \ call JLSetProjectRoot()

    " toggle trailing whitespace on bufwin enter/leave
    autocmd BufWinEnter * match TrailingWhiteSpace /\s\+$/
    autocmd BufWinLeave * call clearmatches()
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
