if vim.fn.executable('ag') == 1 then
  vim.g.ackprg = 'ag --vimgrep --smart-case'
end

-- Search all text and add results tp location-list window
vim.keymap.set('n', '<leader>a', ':Ack!<space>')
-- append search to existing location-list
vim.keymap.set('n', '<leader>aa', ':AckAdd<space>')
-- Search help files and add results to location-list window
vim.keymap.set('n', '<leader>ah', ':AgHelp!<space>')
