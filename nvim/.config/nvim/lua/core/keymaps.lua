local set_map = require('core.util').set_map
local opts = { noremap = true, silent = true }
local map = set_map(opts)
local expr_opts = { noremap = true, silent = true, expr = true }
local expr_map = set_map(expr_opts)

-- the leader key
map('n', '<Space>', '<Nop>')

-- save and quit
map('n', '<leader>sk', ':w<CR><C-l>', 'save current buffer')
map('n', '<leader>sa', ':wa<CR><C-l>', 'save all buffers')
map('n', '<leader>q', ':q<CR>', 'quit')
map('i', 'jk', '<Esc>')

-- backspace replacement
map('i', '<C-h>', '<BS>')

-- windows and buffers
map('n', '<leader>w', '<C-w>', 'windows')
map('n', '<leader><leader>', '<C-^>', 'toggle between buffers')

-- make Y behave like the rest of the capital letters
map('n', 'Y', 'y$')

-- keep it centered
map('n', 'n', 'nzz')
map('n', 'N', 'Nzz')
map('n', '*', '*zz')
map('n', '#', '#zz')

-- rename all occurrences of a word in the current buffer
map('n', '<leader>n', ':%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>', 'rename word')

-- move lines of text
map('n', '<leader>j', ':m .+1<CR>==', 'move one line down')
map('n', '<leader>k', ':m .-2<CR>==', 'move one line up')

-- quickfix list
map('n', '<C-j>', ':cnext<CR>zz')
map('n', '<C-k>', ':cprev<CR>zz')

-- move between wrapped lines
expr_map('n', 'j', 'v:count ? "j" : "gj"')
expr_map('n', 'k', 'v:count ? "k" : "gk"')
