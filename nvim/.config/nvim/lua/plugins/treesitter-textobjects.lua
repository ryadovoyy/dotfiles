local tos = require('nvim-treesitter-textobjects.select')
local set_map = require('core.util').set_map

require('nvim-treesitter-textobjects').setup({
  select = {
    lookahead = true,
  },
})

local map = set_map({})

map({ 'x', 'o' }, 'af', function()
  tos.select_textobject('@function.outer', 'textobjects')
end, 'outer function')

map({ 'x', 'o' }, 'if', function()
  tos.select_textobject('@function.inner', 'textobjects')
end, 'inner function')

map({ 'x', 'o' }, 'ac', function()
  tos.select_textobject('@class.outer', 'textobjects')
end, 'outer class')

map({ 'x', 'o' }, 'ic', function()
  tos.select_textobject('@class.inner', 'textobjects')
end, 'inner class')
