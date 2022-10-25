local keymap_groups = require('which-key').register

require('which-key').setup({
    key_labels = {
        ['<leader>'] = 'SPC',
        ['<space>'] = 'SPC'
    },
    popup_mappings = {
        scroll_down = '<C-j>',
        scroll_up = '<C-k>'
    },
    show_help = false,
    triggers = { '<leader>' }
})

keymap_groups({
    g = { name = 'git', h = { name = 'hunk' } },
    h = { name = 'help' },
    l = { name = 'lsp', e = { name = 'errors' } },
    p = { name = 'telescope' },
    s = { name = 'save' }
}, { prefix = '<leader>' })
