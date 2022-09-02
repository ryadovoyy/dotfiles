local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

local highlight_augroup = augroup('YankHighlight', { clear = true })
local packer_augroup = augroup('Packer', { clear = true })
local indent_augroup = augroup('Indent', { clear = true })

-- highlight on yank
autocmd('TextYankPost', {
    group = highlight_augroup,
    pattern = '*',
    callback = function()
        vim.highlight.on_yank({ timeout = 1000 })
    end
})

-- run PackerSync (PackerUpdate and then PackerCompile) whenever packer.lua is updated
autocmd('BufWritePost', {
    group = packer_augroup,
    pattern = 'packer.lua',
    callback = function()
        vim.cmd('source <afile> | PackerSync')
    end
})

-- don't replace tabs with spaces in specified files
autocmd({ 'BufRead', 'BufNewFile' }, {
    group = indent_augroup,
    pattern = '*.go,*.c,*.h',
    callback = function()
        vim.bo.expandtab = false
    end
})
