" FUNCTIONS {{{
" }}}

" SETTINGS {{{
  set hidden                            " Allow buffers to be hidden with unwritten changes
  set tabstop=2                         " Tabs are 2 space characters
  set shiftwidth=2                      " Indentations are 2 space characters
  set softtabstop=2                     " Tabs in insert mode are 2 space characters
  set expandtab                         " Expand tabs to spaces
  set autoindent                        " Use indentation from previous line
  set smartindent                       " Auto indent based on c-like rules
  set backspace=indent,eol,start        " Allow vim to backspace like normal in insert mode
  set incsearch                         " Begin searching as soon as text is entered
  set hlsearch                          " Highlight search results
  set ignorecase smartcase              " Case insensitive searches unless capital letter used
  set number                            " Show line numbers
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
  set cursorline                        " highlight current line
  set autoread                          " reload files changed outside of vim
  set viminfo^=%                        " Remember info about open buffers on close
  set linebreak                         " do not split up words when wrapping
  set path=**                           " set path to ** off current directory
  set display+=lastline                 " display as much of lastline as possible
  set scrolloff=10                      " minimum rows to keep above/below cursor
  set sidescrolloff=10                  " minimum cols to keep left/right of cursor
  set showcmd                           " Show command that is being typed
  set switchbuf=useopen,usetab          " Use already open buffers when switching buffers
  set formatoptions-=o                  " don't continue comments when pushing o/O
  set dictionary+=/usr/share/dict/words " add unix dictionary
  set complete-=i                       " do not use included files in completion (for speed in big dirs)
  " set lazyredraw                       " only redraw when necessary
  " set encoding=utf-8                  " use utf-8 encoding
  " set mouse=a                         " Allow scrolling and make vim clickable
  " set colorcolumn=80                  " set highlighted column

  " HTML INDENT {{{
    let g:html_indent_inctags = "html,body,head,tbody"
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
" }}}

" MAPPINGS {{{
  " Quickly open and reload vimrc
  nnoremap <leader>ev :vsplit $MYVIMRC<cr>
  nnoremap <leader>sv :source $MYVIMRC<cr>:nohl<cr>

  " Disable Ex mode
  nnoremap Q <nop>

  " Disable man lookup
  nnoremap K <nop>

  " Map space to colon
  nnoremap <space> :

  " select last text visual selected
  " normal gv does this based on line numbers
  " so is inaccurate if the visual line is moved
  nmap gv `[v`]

  " make Y behave like C,D,etc
  nnoremap Y y$
" }}}

" AUTO COMMANDS {{{
  " augroup auto_source_vimrc
  "   autocmd!
  "   " Reload vimrc on edit
  "   autocmd BufWritePost $MYVIMRC source $MYVIMRC | call lightline#highlight()
  " augroup END
" }}}
