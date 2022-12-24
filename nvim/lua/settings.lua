-- global opts
vim.opt.hidden = true                               -- Allow buffers to be hidden with unwritten changes
vim.opt.tabstop = 2                                 -- Tabs are 2 space characters
vim.opt.shiftwidth = 2                              -- Indentations are 2 space characters
vim.opt.softtabstop = 2                             -- Tabs in insert mode are 2 space characters
vim.opt.expandtab = true                            -- Expand tabs to spaces
vim.opt.autoindent = true                           -- Use indentation from previous line
vim.opt.smartindent = true                          -- Auto indent based on c-like rules
vim.opt.number = true                               -- Show line numbers
vim.opt.wrap = false                                -- Do not wrap text
vim.opt.backspace = {'indent', 'eol', 'start'}      -- Allow vim to backspace like normal in insert mode
vim.opt.incsearch =  true                           -- Begin searching as soon as text is entered
vim.opt.hlsearch = false                             -- Highlight search results
vim.opt.ignorecase = true                           -- Case insensitive searches unless capital letter used
vim.opt.smartcase = true                           -- Case insensitive searches unless capital letter used
vim.opt.listchars = {extends = '»' ,precedes = '«'} -- Chars to display on text off screen
vim.opt.showmatch = true                            -- Shows matching {,(,if etc. when typing closing },),end
vim.opt.history = 10000                             -- Set # of commands to keep in history
vim.opt.termguicolors = true                        -- Enable term gui colors
vim.opt.wildignore:append('*.swp,*.class,*.o')      -- Ignore files with these extensions
vim.opt.backupdir:prepend('~/.vim/.backup/')        -- Where to store backup files
vim.opt.directory:prepend('~/.vim/.tmp/')           -- Where to store swap files
vim.opt.splitright = true                           -- open vertical splits to the right
vim.opt.splitbelow = true                           -- open horizontal splits below
vim.opt.timeoutlen = 300                            -- Set key stroke timeout
vim.opt.ttimeoutlen = 10                            -- Set key stroke timeout
vim.opt.wildmenu = true                             -- enable bash style tab completion
vim.opt.wildmode= {list = 'longest', 'full'}        -- enable bash style tab completion
vim.opt.cursorline = true                           -- highlight current line
vim.opt.autoread = true                             -- reload files changed outside of vim
vim.opt.viminfo:prepend('%')                        -- Remember info about open buffers on close
vim.opt.linebreak = true                            -- do not split up words when wrapping
vim.opt.breakindent = true
vim.opt.display:append('lastline')                  -- display as much of lastline as possible
vim.opt.scrolloff = 10                              -- minimum rows to keep above/below cursor
vim.opt.sidescrolloff = 10                          -- minimum cols to keep left/right of cursor
vim.opt.showcmd = true                              -- Show command that is being typed
vim.opt.switchbuf = 'useopen,usetab'                -- Use already open buffers when switching buffers
vim.opt.formatoptions:remove('o')                   -- don't continue comments when pushing o/O
vim.opt.dictionary:append('/usr/share/dict/words')  -- add unix dictionary
vim.opt.complete:remove('i')                        -- do not use included files in completion (for speed in big dirs)
vim.opt.synmaxcol = 200                             -- Prevent syntax highlighting past X columns for performance
vim.opt.mouse = 'a'                                 -- Allow scrolling and make vim clickable
vim.opt.completeopt = {'menu', 'menuone', 'noselect' } -- for nvim-cmp
-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- persist undos across sessions (github/joelhooks/dotfiles)
vim.opt.undodir:prepend('~/.vim/.undo/')
vim.opt.undofile = true

-- use faster search alternatives if available
if vim.fn.executable('ag') == 1 then
  vim.opt.grepprg='ag --nogroup --nocolor --column --smart-case'
  vim.opt.grepformat='%f:%l:%c:%m,%f:%l:%m'
elseif vim.fn.executable('ack') == 1 then
  vim.opt.grepprg='ack --nogroup --nocolor --ignore-case --column'
  vim.opt.grepformat='%f:%l:%c:%m,%f:%l:%m'
end

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})
