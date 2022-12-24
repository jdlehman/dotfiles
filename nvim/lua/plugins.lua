-- automatically compile when plugins.lua is saved
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  -- have packer manage itself
  use 'wbthomason/packer.nvim'

  -- ui
  use {
    'folke/tokyonight.nvim',
    config = function()
      vim.cmd [[colorscheme tokyonight-storm]]
    end
  }
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons' },
    config = function()
      require('lualine').setup {
	options = {
	  theme = 'tokyonight'
	}
      }
    end
  }
  use {
    'akinsho/bufferline.nvim', tag = "v3.*", requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      require('bufferline').setup()
    end
  }

  -- editing
  use 'tpope/vim-surround'
  use {
    'terrortylor/nvim-comment',
    config = function()
      require('nvim_comment').setup()
    end
  }
  use 'tpope/vim-repeat'
  use {
    'mbbill/undotree',
    config = function()
      require('config.undotree')
    end
  }

  -- search
  -- Fuzzy Finder (files, lsp, etc)
  use {
    'nvim-telescope/telescope.nvim', branch = '0.1.x', requires = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('config.telescope')
    end,
  }
  -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
  use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make', cond = vim.fn.executable 'make' == 1 }

  -- formatting
  use 'editorconfig/editorconfig-vim'
  use 'tpope/vim-sleuth' -- auto detect tab spacing
  use {
    "jose-elias-alvarez/null-ls.nvim",
    requires = { "neovim/nvim-lspconfig", "nvim-lua/plenary.nvim" },
    config = function()
      require("config.null-ls")
    end,
  }
  -- Add indentation guides even on blank lines
  use {
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      -- Enable `lukas-reineke/indent-blankline.nvim`
      require('indent_blankline').setup {
	char = '┊',
	show_trailing_blankline_indent = false,
      }
    end,
  }

  -- syntax highlighting
  use {
    'nvim-treesitter/nvim-treesitter',
    run = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
    config = function()
      require('config.treesitter')
    end,
  }
  use { -- Additional text objects via treesitter
    'nvim-treesitter/nvim-treesitter-textobjects',
    after = 'nvim-treesitter',
  }

  -- source control
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  use {
    'lewis6991/gitsigns.nvim',
    tag = 'release',
    config = function()
      require('gitsigns').setup {}
    end
  }

  -- completions
  use {
    'hrsh7th/nvim-cmp',
    requires = {
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
  }

  -- language server
  use({
    "neovim/nvim-lspconfig",
    after = 'nvim-cmp',
    requires = {
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
      require('neodev').setup()
    end
  })

  -- Language specific
  use { 'pangloss/vim-javascript', ft = 'javascript' }
  use { 'keith/swift.vim', ft = 'swift' }
  use {
    'fatih/vim-go',
    ft = 'go',
    run = ':GoUpdateBinaries',
    config = function()
      require('config.vim-go')
    end
  }
  use { 'StanAngeloff/php.vim', ft = 'php' }
  use 'jparise/vim-graphql'
  use 'udalov/kotlin-vim'

   -- Add custom plugins to packer from ~/.config/nvim/lua/custom/plugins.lua
  local has_plugins, plugins = pcall(require, 'custom.plugins')
  if has_plugins then
    plugins(use)
  end

  if packer_bootstrap then
    require('packer').sync()
  end
end)
