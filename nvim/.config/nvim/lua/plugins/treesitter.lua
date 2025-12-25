local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

local parsers = {
  'bash',
  'c',
  'cmake',
  'comment',
  'diff',
  'dockerfile',
  'go',
  'gomod',
  'gosum',
  'gowork',
  'html',
  'http',
  'json',
  'lua',
  'luadoc',
  'make',
  'markdown',
  'regex',
  'toml',
  'vim',
  'vimdoc',
  'yaml',
}

require('nvim-treesitter').install(parsers)

local treesitter_augroup = augroup('Treesitter', { clear = true })

autocmd('FileType', {
  group = treesitter_augroup,
  pattern = parsers,
  callback = function()
    vim.treesitter.start()
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
