-- install lazy
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/lazy/lazy.nvim'

if not vim.loop.fs_stat(install_path) then
  local repo = 'https://github.com/folke/lazy.nvim.git'
  fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    '--branch=stable',
    repo,
    install_path,
  })
end

vim.opt.rtp:prepend(install_path)

local lazy = require('lazy')

-- install plugins
lazy.setup({
  -- lsp
  {
    'neovim/nvim-lspconfig',
    dependencies = {
      { 'williamboman/mason.nvim', config = true },
      'williamboman/mason-lspconfig.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      { 'j-hui/fidget.nvim', enabled = false, opts = {} },
      { 'folke/lazydev.nvim', ft = 'lua', opts = {} },
    },
    config = function()
      require('plugins.lspconfig')
    end,
  },

  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      {
        'L3MON4D3/LuaSnip',
        build = (function()
          if vim.fn.has('win32') == 1 or vim.fn.executable('make') == 0 then
            return
          end
          return 'make install_jsregexp'
        end)(),
        dependencies = {
          {
            'rafamadriz/friendly-snippets',
            config = function()
              require('luasnip.loaders.from_vscode').lazy_load()
            end,
          },
        },
      },
      'saadparwaiz1/cmp_luasnip',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
    },
    config = function()
      require('plugins.cmp')
    end,
  },

  -- formatter
  {
    'stevearc/conform.nvim',
    lazy = false,
    config = function()
      require('plugins.conform')
    end,
  },

  -- linter
  {
    'mfussenegger/nvim-lint',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require('plugins.lint')
    end,
  },

  -- telescope
  {
    'nvim-telescope/telescope.nvim',
    event = 'VimEnter',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = vim.fn.executable('make') == 1,
      },
      'nvim-telescope/telescope-project.nvim',
      'nvim-telescope/telescope-ui-select.nvim',
    },
    config = function()
      require('plugins.telescope')
    end,
  },

  -- treesitter
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function()
      require('plugins.treesitter')
    end,
  },

  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    dependencies = { 'nvim-treesitter' },
  },

  {
    'nvim-treesitter/nvim-treesitter-context',
    dependencies = { 'nvim-treesitter' },
    config = function()
      require('plugins.treesitter-context')
    end,
  },

  -- file explorer
  {
    'nvim-tree/nvim-tree.lua',
    config = function()
      require('plugins.nvim-tree')
    end,
  },

  -- git integration
  {
    'TimUntersberger/neogit',
    config = function()
      require('plugins.neogit')
    end,
  },

  {
    'lewis6991/gitsigns.nvim',
    event = 'BufRead',
    config = function()
      require('plugins.gitsigns')
    end,
  },

  -- keymap panel
  {
    'folke/which-key.nvim',
    event = 'VimEnter',
    config = function()
      require('plugins.which-key')
    end,
  },

  -- statusline
  {
    'nvim-lualine/lualine.nvim',
    config = function()
      require('plugins.lualine')
    end,
  },

  -- devicons
  { 'nvim-tree/nvim-web-devicons', enabled = vim.g.have_nerd_font },

  -- indentation guides
  {
    'lukas-reineke/indent-blankline.nvim',
    main = 'ibl',
    config = function()
      require('plugins.indent-line')
    end,
  },

  -- colorcolumn
  {
    'lukas-reineke/virt-column.nvim',
    config = function()
      require('plugins.virt-column')
    end,
  },

  -- colorscheme
  {
    'folke/tokyonight.nvim',
    priority = 1000,
    config = function()
      require('plugins.tokyonight')
    end,
  },

  -- sessions
  {
    'folke/persistence.nvim',
    config = function()
      require('plugins.persistence')
    end,
  },

  -- autopairs
  {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    dependencies = { 'hrsh7th/nvim-cmp' },
    config = function()
      require('plugins.autopairs')
    end,
  },

  -- typescript integration
  {
    'pmizio/typescript-tools.nvim',
    ft = 'typescript',
    dependencies = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
    config = function()
      require('plugins.typescript')
    end,
  },

  -- improved text editing
  {
    'folke/flash.nvim',
    event = 'VeryLazy',
    config = function()
      require('plugins.flash')
    end,
  },

  { 'numToStr/Comment.nvim', opts = {} },

  'tpope/vim-repeat',
  'tpope/vim-surround',
  'vim-scripts/ReplaceWithRegister',

  -- detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',
})

-- update plugins every 7 days
local update_date_path = fn.stdpath('data') .. '/.last-plugin-update-date'

local function write_current_date()
  local command = 'echo ' .. os.time() .. '>' .. update_date_path
  os.execute(command)
end

if fn.filereadable(update_date_path) == 0 then
  write_current_date()
end

vim.cmd('let g:update_date = readfile("' .. update_date_path .. '")[0]')
local update_date = vim.api.nvim_get_var('update_date')
local day_seconds = 24 * 60 * 60
local update_day = math.floor(tonumber(update_date) / day_seconds)
local current_day = math.floor(os.time() / day_seconds)

if current_day > (update_day + 7) then
  vim.ui.input({
    prompt = 'Auto-update plugins now? (y or n) ',
  }, function(input)
    if input == 'y' then
      lazy.update()
      write_current_date()
    end
  end)
end
