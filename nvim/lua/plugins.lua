-- automatically compile when plugins.lua is saved
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
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
      vim.cmd[[colorscheme tokyonight-storm]]
    end
  }
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons'},
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
      require('config.undo_tree')
    end
  }

  -- formatting
  use 'editorconfig/editorconfig-vim'

  -- syntax highlighting
   use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
   use 'w0rp/ale'

  -- source control
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
      'hrsh7th/cmp-cmdline'
    },
    config = [[require('config.nvim-cmp')]]
  }

  -- language server
  use { "williamboman/mason.nvim" }
  use { "williamboman/mason-lspconfig.nvim" }
  use ({
    "neovim/nvim-lspconfig",
    config = [[require('config.lsp')]],
  })

  -- Language specific
  use { 'pangloss/vim-javascript',  ft = 'javascript' }
  use { 'keith/swift.vim', ft = 'swift' }
  use {
    'fatih/vim-go',
    ft = 'go',
    run = ':GoUpdateBinaries',
    config = [[require('config.vim_go')]]
  }
  use { 'StanAngeloff/php.vim', ft = 'php' }
  use 'jparise/vim-graphql'
  use 'udalov/kotlin-vim'

  if packer_bootstrap then
    require('packer').sync()
  end
end)
