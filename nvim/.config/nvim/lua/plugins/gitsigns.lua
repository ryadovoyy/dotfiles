local status_ok, gitsigns = pcall(require, 'gitsigns')
if not status_ok then
    return
end

local set_map = require('core.util').set_map

gitsigns.setup({
    signs = {
        add          = { hl = 'GitSignsAdd'   , text = '┃' },
        change       = { hl = 'GitSignsChange', text = '┃' },
        delete       = { hl = 'GitSignsDelete', text = '_' },
        topdelete    = { hl = 'GitSignsDelete', text = '‾' },
        changedelete = { hl = 'GitSignsChange', text = '~' }
    },
    preview_config = {
        border = 'none',
        style = 'minimal',
        relative = 'cursor',
        row = 1,
        col = 0
    },
    on_attach = function(bufnr)
        local gs = package.loaded.gitsigns
        local opts = { buffer = bufnr }
        local map = set_map(opts)

        map('n', '<leader>ghv', gs.preview_hunk, 'preview hunk')
        map('n', '<leader>ghn', gs.next_hunk, 'next hunk')
        map('n', '<leader>ghp', gs.prev_hunk, 'previous hunk')
        map('n', '<leader>ghr', gs.reset_hunk, 'revert hunk')
    end
})
