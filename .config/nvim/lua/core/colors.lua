local colorscheme = 'wal'

local status_ok, _ = pcall(vim.cmd.colorscheme, colorscheme)
if not status_ok then
    vim.notify('Colorscheme ' .. colorscheme .. ' not found')
    return
end

local function hl(group, opts)
    vim.api.nvim_set_hl(0, group, opts)
end

local bold = { ['bold'] = true }

-- non-plugin highlight groups
hl('Identifier', { ctermbg = 'none', ctermfg = 1, cterm = {} })
hl('Visual', { ctermbg = 0, ctermfg = 7, cterm = bold })
hl('StatusLine', { ctermbg = 0, ctermfg = 7, cterm = bold })
hl('VertSplit', { ctermbg = 'none', ctermfg = 2 })
hl('ColorColumn', { ctermbg = 0, ctermfg = 7 })
hl('Pmenu', { ctermbg = 'none', ctermfg = 7 })
hl('DiffAdd', { ctermfg = 107 })
hl('DiffDelete', { ctermfg = 167 })
hl('vimUserCommand', { ctermfg = 1, cterm = {} })
hl('htmlTagName', { ctermfg = 1, cterm = {} })

-- plugin highlight groups
hl('TelescopeMatching', { ctermfg = 1, cterm = bold })
hl('TelescopePromptCounter', { ctermfg = 8 })
hl('TelescopePromptPrefix', { ctermbg = 'none', ctermfg = 11, cterm = bold })
hl('TelescopeSelectionCaret', { ctermbg = 0, ctermfg = 11, cterm = bold })
hl('GitSignsAdd', { ctermfg = 107 })
hl('GitSignsDelete', { ctermfg = 167 })
