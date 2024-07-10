local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

local yank_augroup = augroup('YankHighlight', { clear = true })
local trail_augroup = augroup('TrailHighlight', { clear = true })
local sidebar_augroup = augroup('SidebarHighlight', { clear = true })

-- highlight on yank
autocmd('TextYankPost', {
  group = yank_augroup,
  pattern = '*',
  callback = function()
    vim.highlight.on_yank({ timeout = 300 })
  end,
})

-- highlight trailing whitespaces
local function exec_match_cmd(command)
  local ignored_filetypes = { 'TelescopePrompt' }
  if vim.tbl_contains(ignored_filetypes, vim.bo.filetype) then
    return
  end
  vim.cmd(command)
end

autocmd('InsertLeave', {
  group = trail_augroup,
  pattern = '*',
  callback = function()
    exec_match_cmd([[match ExtraWhitespace /\s\+$/]])
  end,
})

autocmd('InsertEnter', {
  group = trail_augroup,
  pattern = '*',
  callback = function()
    exec_match_cmd([[match ExtraWhitespace /\s\+\%#\@<!$/]])
  end,
})

-- better sidebar highlighting
autocmd({ 'BufEnter', 'BufWinEnter' }, {
  group = sidebar_augroup,
  pattern = '*',
  callback = function()
    local sidebars = { 'NvimTree', 'qf', 'help', 'man' }

    if vim.tbl_contains(sidebars, vim.bo.filetype) then
      local hl = table.concat({
        'Normal:NormalSB',
        'SignColumn:SignColumnSB',
        'EndOfBuffer:NvimTreeEndOfBuffer',
        'WinSeparator:NvimTreeWinSeparator',
      }, ',')
      vim.wo.winhl = hl
      vim.wo.colorcolumn = ''
    else
      if string.match(vim.wo.winhl, 'Telescope') then
        local hl = table.concat({
          'Normal:TelescopePromptNormal',
          'EndOfBuffer:TelescopePromptNormal',
        }, ',')
        vim.wo.winhl = hl
      else
        vim.wo.winhl = ''
      end
      vim.wo.colorcolumn = '80'
    end
  end,
})
