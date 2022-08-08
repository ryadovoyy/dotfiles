local o = vim.o
local opt = vim.opt
local g = vim.g

o.tabstop = 4
o.softtabstop = 4
o.shiftwidth = 4
o.expandtab = true
o.smartindent = true

o.hidden = true
o.updatetime = 100
o.regexpengine = 0
o.wrap = false
o.signcolumn = 'yes'
o.number = true
o.relativenumber = true

-- visualise tabs
o.list = true
o.listchars = 'tab:│ '

o.laststatus = 3
o.showmode = false
o.colorcolumn = '80'
o.guicursor = ''
o.scrolloff = 8
o.incsearch = true
o.hlsearch = false
o.background = 'dark'
o.errorbells = false

-- don't give |ins-completion-menu| messages
opt.shortmess:append({ c = true })

o.swapfile = false
o.undofile = false
o.backup = false

g.mapleader = ' '

-- fix netrw error
g.netrw_keepdir = 0
