local o = vim.o
local opt = vim.opt
local g = vim.g

o.tabstop = 4
o.softtabstop = 4
o.shiftwidth = 4
o.expandtab = true
o.smartindent = true

o.hidden = true
o.clipboard = 'unnamedplus'
o.updatetime = 100
o.timeoutlen = 500
o.regexpengine = 0
o.wrap = true
o.signcolumn = 'yes'
o.number = true
o.relativenumber = true

-- visualise tabs
o.list = true
opt.listchars = { tab = '▎ ' }

-- split separators
o.fillchars = 'horiz:━,horizup:┻,horizdown:┳,vert:┃,vertleft:┫,vertright:┣,verthoriz:╋'

o.laststatus = 3
o.splitright = true
o.splitbelow = true
o.showmode = false
o.ruler = false
o.colorcolumn = '80'
o.cursorline = true
o.guicursor = ''
o.scrolloff = 8
o.ignorecase = true
o.smartcase = true
o.inccommand = 'split'
o.incsearch = true
o.hlsearch = true
o.termguicolors = true
o.background = 'dark'
o.errorbells = false

-- don't give |ins-completion-menu| messages
opt.shortmess:append({ c = true })

o.swapfile = false
o.undofile = false
o.backup = false

g.mapleader = ' '
g.have_nerd_font = true
