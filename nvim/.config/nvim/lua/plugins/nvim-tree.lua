local node = require('nvim-tree.api').node
local set_map = require('core.util').set_map

require('nvim-tree').setup({
    disable_netrw = true,
    hijack_cursor = true,
    view = {
        adaptive_size = true,
        hide_root_folder = true
    },
    renderer = {
        icons = { webdev_colors = false }
    },
    actions = {
        open_file = {
            window_picker = { enable = false }
        }
    },
    git = { enable = false },
    on_attach = function(bufnr)
        local opts = { buffer = bufnr }
        local map = set_map(opts)
        map('n', 'l', node.open.edit)
        map('n', 'h', node.navigate.parent_close)
    end
})

local opts = { noremap = true, silent = true }
local map = set_map(opts)
map('n', '<leader>n', ':NvimTreeToggle<CR>')
