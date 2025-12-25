-- text editing
require('mini.operators').setup({
  sort = {
    prefix = 'go',
  },
})

require('mini.surround').setup({
  mappings = {
    add = 'gsa',
    delete = 'gsd',
    find = 'gsf',
    find_left = 'gsF',
    highlight = 'gsh',
    replace = 'gsr',
  },
})

require('mini.comment').setup()
require('mini.pairs').setup()
require('mini.splitjoin').setup()

-- general workflow
require('mini.bufremove').setup()
