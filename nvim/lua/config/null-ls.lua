local null_ls = require("null-ls")
null_ls.setup({
  sources = {
    -- Python
    null_ls.builtins.formatting.autopep8,
    null_ls.builtins.formatting.isort,

    -- JS yaml html markdown
    null_ls.builtins.formatting.prettier,

    -- Markdown
    null_ls.builtins.diagnostics.markdownlint,

    -- Spell checking
    null_ls.builtins.diagnostics.codespell.with({
      args = { "--builtin", "clear,rare,code", "-" },
    }),
  },
  on_attach = function()
    vim.cmd([[ command! Format execute 'lua vim.lsp.buf.format { async = true }' ]])
  end,
})
