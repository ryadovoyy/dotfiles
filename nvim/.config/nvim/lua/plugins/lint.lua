local lint = require('lint')
local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

lint.linters_by_ft = {
  dockerfile = { 'hadolint' },
}

local lint_augroup = augroup('Lint', { clear = true })

autocmd({ 'BufEnter', 'BufWritePost', 'TextChanged' }, {
  group = lint_augroup,
  pattern = '*Dockerfile*',
  callback = function()
    if vim.bo.modifiable then
      lint.try_lint()
    end
  end,
})
