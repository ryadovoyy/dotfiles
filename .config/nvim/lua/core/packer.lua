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
return packer.startup(function(use)
    use('wbthomason/packer.nvim')

    -- essential nvim plugins
    use({
        'neovim/nvim-lspconfig',
        event = 'BufRead',
        config = function()
            require('plugins.lspconfig')
        end
    })

    use({
        'nvim-telescope/telescope.nvim',
        tag = '0.1.0',
        requires = { 'nvim-lua/plenary.nvim' },
        config = function()
            require('plugins.telescope')
        end
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
        'nvim-treesitter/nvim-treesitter',
        run = function()
            require('nvim-treesitter.install').update({ with_sync = true })
        end,
        config = function()
            require('plugins.treesitter')
        end
    })

    -- git integration
    use({
        'lewis6991/gitsigns.nvim',
        event = 'BufRead',
        config = function()
            require('plugins.gitsigns')
        end
    })

    -- statusline
    use({
        'nvim-lualine/lualine.nvim',
        event = 'BufRead',
        config = function()
            require('plugins.lualine')
        end
    })

    -- devicons
    use({
        'kyazdani42/nvim-web-devicons',
        config = function()
            require('nvim-web-devicons').setup()
        end
    })

    -- improved text editing
    use('tpope/vim-repeat')
    use('tpope/vim-surround')
    use('tpope/vim-commentary')
    use('vim-scripts/ReplaceWithRegister')

    -- colorscheme
    use('dylanaraps/wal.vim')

    if packer_bootstrap then
        require('packer').sync()
    end
end)
