-- install lazy
local install_path = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'

if not (vim.uv or vim.loop).fs_stat(install_path) then
  local repo = 'https://github.com/folke/lazy.nvim.git'

  local out = vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    '--branch=stable',
    repo,
    install_path,
  })

  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { 'Failed to clone lazy.nvim:\n', 'ErrorMsg' },
      { out, 'WarningMsg' },
      { '\nPress any key to exit...' },
    }, true, {})

    vim.fn.getchar()
    os.exit(1)
  end
end

vim.opt.rtp:prepend(install_path)

local lazy = require('lazy')

-- install plugins
lazy.setup({
  -- snacks
  {
    'folke/snacks.nvim',
    priority = 1000,
    lazy = false,
    config = function()
      require('plugins.snacks')
    end,
  },

  -- mini
  {
    'nvim-mini/mini.nvim',
    version = false,
    config = function()
      require('plugins.mini')
    end,
  },

  -- lsp
  {
    'neovim/nvim-lspconfig',
    dependencies = {
      { 'folke/lazydev.nvim', ft = 'lua', config = true },
      { 'mason-org/mason.nvim', config = true },
      'mason-org/mason-lspconfig.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      'saghen/blink.cmp',
    },
    config = function()
      require('plugins.lspconfig')
    end,
  },

  {
    'saghen/blink.cmp',
    event = 'VimEnter',
    version = '1.*',
    dependencies = {
      {
        'L3MON4D3/LuaSnip',
        version = '2.*',
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
        config = true,
      },
    },
    config = function()
      require('plugins.blink')
    end,
  },

  -- formatter
  {
    'stevearc/conform.nvim',
    event = { 'BufWritePre' },
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

  -- treesitter
  {
    'nvim-treesitter/nvim-treesitter',
    lazy = false,
    build = ':TSUpdate',
    config = function()
      require('plugins.treesitter')
    end,
  },

  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    branch = 'main',
    config = function()
      require('plugins.treesitter-textobjects')
    end,
  },

  {
    'nvim-treesitter/nvim-treesitter-context',
    config = function()
      require('plugins.treesitter-context')
    end,
  },

  -- git integration
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
    event = 'VeryLazy',
    config = function()
      require('plugins.which-key')
    end,
  },

  -- statusline
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('plugins.lualine')
    end,
  },

  -- devicons
  { 'nvim-tree/nvim-web-devicons', enabled = vim.g.have_nerd_font },

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
    lazy = false,
    config = function()
      require('plugins.tokyonight')
    end,
  },

  -- sessions
  {
    'folke/persistence.nvim',
    event = 'BufReadPre',
    config = function()
      require('plugins.persistence')
    end,
  },

  -- blazingly fast navigation
  {
    'folke/flash.nvim',
    event = 'VeryLazy',
    config = function()
      require('plugins.flash')
    end,
  },

  -- repeat supported plugin keymaps
  'tpope/vim-repeat',

  -- detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',
})

-- update plugins every 7 days
local update_date_path = vim.fn.stdpath('data') .. '/.last-plugin-update-date'

local function write_current_date()
  local command = 'echo ' .. os.time() .. ' > ' .. update_date_path
  os.execute(command)
end

if vim.fn.filereadable(update_date_path) == 0 then
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
