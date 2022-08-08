local tsb = require('telescope.builtin')
local set_map = require('core.util').set_map

require('telescope').setup({
    defaults = {
        color_devicons = false,
        mappings = {
            i = {
                ['<C-j>'] = 'move_selection_next',
                ['<C-k>'] = 'move_selection_previous'
            }
        }
    }
})

local opts = { noremap = true }
local map = set_map(opts)

map('n', '<C-p>', tsb.find_files)
map('n', '<C-g>', tsb.git_files)
map('n', '<C-b>', tsb.buffers)
map('n', '<C-f>', tsb.live_grep)
map('n', '<C-h>', tsb.help_tags)
map('n', '<leader>gd', tsb.lsp_definitions)
map('n', '<leader>gr', tsb.lsp_references)
map('n', '<leader>fd', tsb.diagnostics)
