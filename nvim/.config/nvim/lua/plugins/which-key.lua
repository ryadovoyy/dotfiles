local which_key = require('which-key')
local set_map = require('core.util').set_map

which_key.setup({
  delay = 500,
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
  { '<leader>lc', group = 'calls' },
  { '<leader>p', group = 'picker' },
  { '<leader>s', group = 'save' },
})

local opts = { noremap = true }
local map = set_map(opts)

map('n', '<leader>?', function()
  which_key.show({ global = false })
end, 'buffer local keymaps')
