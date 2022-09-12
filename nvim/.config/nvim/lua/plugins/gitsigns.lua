local set_map = require('core.util').set_map

require('gitsigns').setup({
    signs = {
        add          = { hl = 'GitSignsAdd'   , text = '│' },
        change       = { hl = 'GitSignsChange', text = '│' },
        delete       = { hl = 'GitSignsDelete', text = '_' },
        topdelete    = { hl = 'GitSignsDelete', text = '‾' },
        changedelete = { hl = 'GitSignsChange', text = '~' }
    },
    preview_config = {
        border = 'rounded',
        style = 'minimal',
        relative = 'cursor',
        row = 1,
        col = 0
    },
    on_attach = function(bufnr)
        local gs = package.loaded.gitsigns
        local opts = { buffer = bufnr }
        local map = set_map(opts)
        map('n', '<leader>hp', gs.preview_hunk)
    end
})