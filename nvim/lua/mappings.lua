vim.g.mapleader = ","

-- modes
-- escape insert mode
vim.keymap.set('i', 'jk', '<esc>')
-- escape visual mode
vim.keymap.set('v', '<space>', '<esc>')

-- movement
-- move up and down by screen line, not file line
-- makes dealing with line wrapping easier
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')
vim.keymap.set('v', 'j', 'gj')
vim.keymap.set('v', 'k', 'gk')

  -- Arrow keys resize current viewport
vim.keymap.set('n', '<left>', ':vertical resize -5<cr>')
vim.keymap.set('n', '<right>', ':vertical resize +5<cr>')
vim.keymap.set('n', '<up>', ':resize +5<cr>')
vim.keymap.set('n', '<down>', ':resize -5<cr>')

  -- move visual blocks of text
vim.keymap.set('v', '<left>', '<gv^')
vim.keymap.set('v', '<right>', '>gv^')
vim.keymap.set('v', '<up>', 'xkP`[V`]')
vim.keymap.set('v', '<down>', 'xp`[V`]')

-- select last text visual selected
-- normal gv does this based on line numbers
-- so is inaccurate if the visual line is moved
vim.keymap.set('n', 'gv', '`[v`]')

-- make Y behave like C,D,etc
vim.keymap.set('n', 'Y', 'y$')

-- Toggle paste mode on and off
vim.keymap.set('', '<leader>pp', ':setlocal paste!<cr>')

-- Quickly open and reload vimrc
vim.keymap.set('n', '<leader>ev', ':vsplit $MYVIMRC<cr>')
vim.keymap.set('n', '<leader>sv', ':source $MYVIMRC<cr>:nohl<cr>')

-- buffers
-- open splits (v vertical, s horizontal)
vim.keymap.set('n', '<leader>v', '<c-w>v')
vim.keymap.set('n', '<leader>s', '<c-w>s')
vim.keymap.set('n', '<c-h>', '<c-w>h')
vim.keymap.set('n', '<c-j>', '<c-w>j')
vim.keymap.set('n', '<c-k>', '<c-w>k')
vim.keymap.set('n', '<c-l>', '<c-w>l')
-- fix issue in terminal updates that cause <c-h> to be <bs>
vim.keymap.set('n', '<bs>', '<c-w>h')
-- see open buffers and select one
vim.keymap.set('n', '<leader>l', ':ls<cr>:b<space>')
  -- switch to alternate file
vim.keymap.set('n', '<leader><leader>', '<c-^>')

-- tabs
-- Open new tabs
vim.keymap.set('n', '<leader>t', ':tabnew<cr>')
-- Switch between tabs is gt
-- open current split in new tab
vim.keymap.set('n', '<leader>nt', '<c-w>T')

-- clipboard
-- copy to system clipboard
vim.keymap.set('n', '<leader>y', '"*y')
vim.keymap.set('v', '<leader>y', '"*y')
-- Paste from the system clipboard using paste mode
vim.keymap.set('n', '<leader>p', ':set paste<cr>"*p<cr>:set nopaste<cr>')
vim.keymap.set('v', '<leader>p', ':set paste<cr>"*p<cr>:set nopaste<cr>')

-- toggle word wrap
vim.keymap.set('n', '<leader>w', ':setlocal wrap!<cr>')

-- toggle spell check
vim.keymap.set('n', '<leader>sp', ':setlocal spell!<cr>')

-- Turn off highlight
vim.keymap.set('n', '<leader>h', ':nohl<cr>')

-- Remove trailing spaces
-- vim.keymap.set('n', '<leader><space>', ':%s/\s\+$<cr>``')

  -- Fix indent on code block (paragraph)
vim.keymap.set('n', '<leader>=', '=ip')

-- use magic regex by default
-- don't have to escape everything anymore
vim.keymap.set('n', '/', '/\v')
vim.keymap.set('v', '/', '/\v')
