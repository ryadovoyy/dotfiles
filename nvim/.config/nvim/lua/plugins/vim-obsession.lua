local set_map = require('core.util').set_map
local opts = { noremap = true }
local map = set_map(opts)
map('n', '<leader>sd', ':Obsess<CR>', 'save desktop')
