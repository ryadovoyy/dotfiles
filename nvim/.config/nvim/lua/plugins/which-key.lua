local which_key = require('which-key')

which_key.setup({
  keys = {
    scroll_down = '<C-j>',
    scroll_up = '<C-k>',
  },
  show_help = false,
})

-- keymap groups
which_key.add({
  { '<leader>g', group = 'git' },
  { '<leader>gh', group = 'hunk' },
  { '<leader>h', group = 'help' },
  { '<leader>l', group = 'lsp' },
  { '<leader>le', group = 'errors' },
  { '<leader>p', group = 'telescope' },
  { '<leader>s', group = 'save' },
})
