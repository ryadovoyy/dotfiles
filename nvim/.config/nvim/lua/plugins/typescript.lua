local set_map = require('core.util').set_map

require('typescript-tools').setup({})

local opts = { noremap = true, silent = true }
local map = set_map(opts)
map('n', '<leader>lt', ':TSToolsOrganizeImports<CR>', 'organize typescript imports')
