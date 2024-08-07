local lint = require('lint')
local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

lint.linters_by_ft = {
  typescript = { 'eslint_d' },
  dockerfile = { 'hadolint' },
}

local lint_augroup = augroup('Lint', { clear = true })

autocmd({ 'BufEnter', 'BufWritePost', 'TextChanged' }, {
  group = lint_augroup,
  pattern = '*.ts,Dockerfile',
  callback = function()
    lint.try_lint()
  end,
})
