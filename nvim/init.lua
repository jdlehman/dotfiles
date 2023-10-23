-- base neovim settings
require('config.options')
-- lazy.nvim plugin manager
require("config.lazy")

vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      -- autocommand
      require("config.autocommands")
      -- key mappings
      require('config.keymaps')
    end,
})
