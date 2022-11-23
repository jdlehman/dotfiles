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

  -- syntax highlighting
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  -- source control
  use {
    'lewis6991/gitsigns.nvim',
    tag = 'release',
    config = function()
      require('gitsigns').setup {}
    end
  }

  -- completions
  use { 'L3MON4D3/LuaSnip' }
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      'saadparwaiz1/cmp_luasnip',
    },
    config = function()
      require('config.nvim-cmp')
    end
  }

  -- language server
  use { "williamboman/mason.nvim" }
  use { "williamboman/mason-lspconfig.nvim" }
  use({
    "neovim/nvim-lspconfig",
    after = 'nvim-cmp',
    config = function()
      require('config.lsp')
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

  if packer_bootstrap then
    require('packer').sync()
  end
end)
