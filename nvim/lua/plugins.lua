return {
  -- ui
  {
    'folke/tokyonight.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd [[colorscheme tokyonight-storm]]
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies  = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('lualine').setup {
	options = {
	  theme = 'tokyonight'
	}
      }
    end
  },
  {
    'akinsho/bufferline.nvim',
    version = "*",
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      vim.opt.termguicolors = true
      require("bufferline").setup{}
    end
  },

  -- editing
  'tpope/vim-surround',
  {
    'terrortylor/nvim-comment',
    keys = {
      { "gcc" },
    },
    main = 'nvim_comment',
    config = true,
  },
  'tpope/vim-repeat',
  {
    'mbbill/undotree',
    keys = { "<leader>u" },
    config = function()
      require('config.undotree')
    end
  },

  -- search
  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies  = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('config.telescope')
    end,
  },
  -- Fuzzy Finder Algorithm which dependencies  local dependencies to be built. Only load if `make` is available
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
    cond = vim.fn.executable 'make' == 1
  },

  -- formatting
  'editorconfig/editorconfig-vim',
  'tpope/vim-sleuth', -- auto detect tab spacing
  {
    "jose-elias-alvarez/null-ls.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies  = { "neovim/nvim-lspconfig", "nvim-lua/plenary.nvim" },
    config = function()
      require("config.null-ls")
    end,
  },
  -- Add indentation guides even on blank lines
  { "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} },

  -- syntax highlighting
  {
    'nvim-treesitter/nvim-treesitter',
    build = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
    config = function()
      require('config.treesitter')
    end,
  },
  { -- Additional text objects via treesitter
    'nvim-treesitter/nvim-treesitter-textobjects',
    dependencies = { 'nvim-treesitter' },
  },

  -- source control
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',
  {
    'lewis6991/gitsigns.nvim',
    event = "BufReadPre",
    config = true,
  },
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewToggleFiles", "DiffviewFocusFiles" },
  },

  -- completions
  {
    'hrsh7th/nvim-cmp',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      'nvim-lspconfig',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      'L3MON4D3/LuaSnip' ,
      'saadparwaiz1/cmp_luasnip',
    },
    config = function()
      require('config.nvim-cmp')
    end
  },

  { 'j-hui/fidget.nvim', tag = 'legacy', event = "LspAttach" },
  -- language server
  {
    "neovim/nvim-lspconfig",
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies  = {
      -- install/manage lsps
      "williamboman/mason.nvim" ,
      "williamboman/mason-lspconfig.nvim" ,
      -- useful status updates
      'j-hui/fidget.nvim',
      -- Additional lua configuration for nvim
      'folke/neodev.nvim',
    },
    config = function()
      require('config.lsp')
    end
  },

  -- Language specific
  { 'pangloss/vim-javascript', ft = 'javascript' },
  { 'keith/swift.vim', ft = 'swift' },
  {
    'fatih/vim-go',
    ft = 'go',
    build = ':GoUpdateBinaries',
    config = function()
      require('config.vim-go')
    end
  },
  { 'StanAngeloff/php.vim', ft = 'php' },
  'jparise/vim-graphql',
  { 'udalov/kotlin-vim', ft = 'kotlin' }
}

