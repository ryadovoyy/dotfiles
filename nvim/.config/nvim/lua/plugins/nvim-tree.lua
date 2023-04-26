local status_ok, nvim_tree = pcall(require, 'nvim-tree')
if not status_ok then
    return
end

local node = require('nvim-tree.api').node
local set_map = require('core.util').set_map

nvim_tree.setup({
    disable_netrw = true,
    hijack_cursor = true,
    sync_root_with_cwd = true,
    view = { adaptive_size = true },
    renderer = {
        root_folder_label = false,
        icons = {
            glyphs = {
                git = {
                    unstaged = '┃',
                    untracked = '┃'
                }
            }
        }
    },
    actions = {
        open_file = {
            window_picker = { enable = false }
        }
    },
    on_attach = function(bufnr)
        local opts = { buffer = bufnr }
        local map = set_map(opts)
        map('n', 'l', node.open.edit)
        map('n', 'h', node.navigate.parent_close)
    end
})

local opts = { noremap = true, silent = true }
local map = set_map(opts)
map('n', '<leader>d', ':NvimTreeToggle<CR>', 'toggle nvim tree')
