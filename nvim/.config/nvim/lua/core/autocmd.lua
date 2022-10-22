local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

local yank_augroup = augroup('YankHighlight', { clear = true })
local trail_augroup = augroup('TrailHighlight', { clear = true })
local packer_augroup = augroup('Packer', { clear = true })
local indent_augroup = augroup('Indent', { clear = true })

-- highlight on yank
autocmd('TextYankPost', {
    group = yank_augroup,
    pattern = '*',
    callback = function()
        vim.highlight.on_yank({ timeout = 300 })
    end
})

-- highlight trailing whitespaces
local function exec_match_cmd(command)
    local ignored_filetypes = { 'TelescopePrompt' }
    if vim.tbl_contains(ignored_filetypes, vim.bo.filetype) then
        return
    end
    vim.cmd(command)
end

autocmd({ 'BufWinEnter', 'InsertLeave' }, {
    group = trail_augroup,
    pattern = '*',
    callback = function()
        exec_match_cmd([[match ExtraWhitespace /\s\+$/]])
    end
})

autocmd('InsertEnter', {
    group = trail_augroup,
    pattern = '*',
    callback = function()
        exec_match_cmd([[match ExtraWhitespace /\s\+\%#\@<!$/]])
    end
})

autocmd('BufWinLeave', {
    group = trail_augroup,
    pattern = '*',
    callback = function()
        exec_match_cmd('call clearmatches()')
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
