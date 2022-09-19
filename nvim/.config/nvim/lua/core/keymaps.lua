local set_map = require('core.util').set_map
local opts = { noremap = true, silent = true }
local map = set_map(opts)
local expr_opts = { noremap = true, silent = true, expr = true }
local expr_map = set_map(expr_opts)

-- the leader key
map('n', '<Space>', '<Nop>')

-- save and quit
map('n', '<leader>ss', ':w<CR><C-l>')
map('n', '<leader>sa', ':wa<CR><C-l>')
map('n', '<leader>q', ':q<CR>')
map('i', 'jk', '<Esc>')

-- backspace replacement
map('i', '<C-h>', '<BS>')

-- windows
map('n', '<leader>w', '<C-w>')

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

-- move between wrapped lines
expr_map('n', 'j', 'v:count ? "j" : "gj"')
expr_map('n', 'k', 'v:count ? "k" : "gk"')
