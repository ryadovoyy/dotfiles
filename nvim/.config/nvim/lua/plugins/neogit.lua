local status_ok, neogit = pcall(require, 'neogit')
if not status_ok then
    return
end

local set_map = require('core.util').set_map

neogit.setup({
    disable_context_highlighting = true,
    use_magit_keybindings = true,
    kind = 'replace',
    signs = {
        section = { '', '' },
        item = { '', '' },
        hunk = { '', '' }
    }
})

local opts = { noremap = true, silent = true }
local map = set_map(opts)
map('n', '<leader>gs', ':Neogit<CR>', 'neogit status')
