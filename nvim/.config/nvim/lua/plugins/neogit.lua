local set_map = require('core.util').set_map

require('neogit').setup({
    disable_context_highlighting = true,
    use_magit_keybindings = true,
    kind = 'replace',
    signs = {
        section = { '', '' },
        item = { '', '' },
        hunk = { '', '' },
    },
})

local opts = { noremap = true, silent = true }
local map = set_map(opts)
map('n', '<leader>gs', ':Neogit<CR>', 'neogit status')
