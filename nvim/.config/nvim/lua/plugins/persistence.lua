local persistence = require('persistence')
local set_map = require('core.util').set_map

persistence.setup({
  branch = false,
})

local opts = { noremap = true }
local map = set_map(opts)
map('n', '<leader>sr', persistence.load, 'restore desktop')
