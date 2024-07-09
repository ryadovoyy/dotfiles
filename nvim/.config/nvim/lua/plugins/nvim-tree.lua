local api = require('nvim-tree.api')
local set_map = require('core.util').set_map

require('nvim-tree').setup({
    disable_netrw = true,
    hijack_cursor = true,
    sync_root_with_cwd = true,
    view = { adaptive_size = true },
    renderer = {
        root_folder_label = false,
        icons = {
            glyphs = {
                git = {
                    unstaged = '󰧞',
                    untracked = '󰧞',
                    deleted = '󰧞',
                },
            },
        },
    },
    actions = {
        open_file = {
            window_picker = { enable = false },
        },
    },
    on_attach = function(bufnr)
        api.config.mappings.default_on_attach(bufnr)

        local opts = { buffer = bufnr }
        local map = set_map(opts)

        map('n', 'l', api.node.open.edit)
        map('n', 'h', api.node.navigate.parent_close)
    end,
})

local opts = { noremap = true, silent = true }
local map = set_map(opts)
map('n', '<leader>d', ':NvimTreeToggle<CR>', 'toggle nvim tree')
