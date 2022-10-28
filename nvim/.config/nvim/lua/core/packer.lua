-- install packer
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({
        'git',
        'clone',
        '--depth',
        '1',
        'https://github.com/wbthomason/packer.nvim',
        install_path
    })
    vim.cmd('packadd packer.nvim')
end

-- use a protected call to avoid error on the first use
local status_ok, packer = pcall(require, 'packer')
if not status_ok then
    return
end

-- have packer use a popup window for command outputs
packer.init({
    display = {
        open_fn = function()
            return require('packer.util').float({ border = 'rounded' })
        end
    }
})

-- install plugins
packer.startup(function(use)
    use('wbthomason/packer.nvim')

    -- essential nvim plugins
    use({
        'neovim/nvim-lspconfig',
        event = 'BufReadPre',
        config = function()
            require('plugins.lspconfig')
        end
    })

    use({
        'nvim-telescope/telescope.nvim',
        branch = '0.1.x',
        requires = { 'nvim-lua/plenary.nvim' }
    })

    use({
        'nvim-telescope/telescope-fzf-native.nvim',
        after = 'telescope.nvim',
        run = 'make',
        cond = vim.fn.executable('make') == 1,
        config = function()
            require('telescope').load_extension('fzf')
        end
    })

    use({
        'nvim-telescope/telescope-project.nvim',
        after = 'telescope.nvim',
        config = function()
            require('telescope').load_extension('project')
            require('plugins.telescope')
        end
    })

    use({
        'nvim-treesitter/nvim-treesitter',
        run = function()
            require('nvim-treesitter.install').update({ with_sync = true })
        end,
        config = function()
            require('plugins.treesitter')
        end
    })

    -- additional textobjects for treesitter
    use({
        'nvim-treesitter/nvim-treesitter-textobjects',
        after = 'nvim-treesitter'
    })

    -- code context
    use({
        'nvim-treesitter/nvim-treesitter-context',
        after = 'nvim-treesitter',
        config = function()
            require('plugins.treesitter-context')
        end
    })

    -- file explorer
    use({
        'nvim-tree/nvim-tree.lua',
        config = function()
            require('plugins.nvim-tree')
        end
    })

    -- git integration
    use({
        'TimUntersberger/neogit',
        config = function()
            require('plugins.neogit')
        end
    })

    use({
        'lewis6991/gitsigns.nvim',
        event = 'BufRead',
        config = function()
            require('plugins.gitsigns')
        end
    })

    -- keymap panel
    use({
        'folke/which-key.nvim',
        config = function()
            require('plugins.which-key')
        end
    })

    -- statusline
    use({
        'nvim-lualine/lualine.nvim',
        config = function()
            require('plugins.lualine')
        end
    })

    -- devicons
    use({
        'nvim-tree/nvim-web-devicons',
        config = function()
            require('nvim-web-devicons').setup()
        end
    })

    -- colorcolumn
    use({
        'lukas-reineke/virt-column.nvim',
        config = function()
            require('plugins.virt-column')
        end
    })

    -- colorscheme
    use({
        'folke/tokyonight.nvim',
        config = function()
            require('plugins.tokyonight')
        end
    })

    -- sessions
    use({
        'tpope/vim-obsession',
        config = function()
            require('plugins.vim-obsession')
        end
    })

    -- improved text editing
    use('tpope/vim-repeat')
    use('tpope/vim-surround')
    use('tpope/vim-commentary')
    use('vim-scripts/ReplaceWithRegister')

    if packer_bootstrap then
        require('packer').sync()
    end
end)

-- update plugins every 7 days
local update_date_path = fn.stdpath('data') .. '/.last-package-update-date'

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
    packer.update()
    write_current_date()
end
