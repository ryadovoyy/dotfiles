local set_map = require('core.util').set_map
local opts = { noremap = true, silent = true }
local map = set_map(opts)

-- the leader key
map('n', '<Space>', '<Nop>')

-- save and quit
map('n', '<leader>s', ':w<CR><C-l>')
map('n', '<leader>q', ':q<CR>')
map('i', 'jk', '<Esc>')

-- system clipboard
map('n', '<leader>y', '"+y')
map('n', '<leader>Y', '"+y$')
map('n', '<leader>p', '"+p')
map('n', '<leader>P', '"+P')

-- toggle between buffers
map('n', '<leader><leader>', '<C-^>')

-- make Y behave like the rest of the capital letters
map('n', 'Y', 'y$')

-- keep it centered
map('n', 'n', 'nzz')
map('n', 'N', 'Nzz')
map('n', '*', '*zz')
map('n', '#', '#zz')

-- move lines of text
map('n', '<leader>j', ':m .+1<CR>==')
map('n', '<leader>k', ':m .-2<CR>==')

-- quickfix list
map('n', '<C-j>', ':cnext<CR>zz')
map('n', '<C-k>', ':cprev<CR>zz')
map('n', '<leader>cc', ':ccl<CR>')
