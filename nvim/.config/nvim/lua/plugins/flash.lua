local flash = require('flash')
local set_map = require('core.util').set_map

flash.setup({})
local map = set_map({})

map({ 'n', 'x', 'o' }, 's', flash.jump, 'flash')
map({ 'n', 'x', 'o' }, 'S', flash.treesitter, 'flash treesitter')
map('o', 'r', flash.remote, 'remote flash')
map({ 'o', 'x' }, 'R', flash.treesitter_search, 'treesitter search')
map('c', '<C-s>', flash.toggle, 'toggle flash search')
