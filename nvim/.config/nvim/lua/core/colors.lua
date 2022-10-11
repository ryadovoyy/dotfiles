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
hl('NonText', { ctermbg = 'none', ctermfg = 8 })
hl('Identifier', { ctermbg = 'none', ctermfg = 1, cterm = {} })
hl('IncSearch', { ctermbg = 8, ctermfg = 7 })
hl('Search', { ctermbg = 8, ctermfg = 7 })
hl('Visual', { ctermbg = 8, ctermfg = 7, cterm = bold })
hl('StatusLine', { ctermbg = 0, ctermfg = 7, cterm = bold })
hl('VertSplit', { ctermbg = 'none', ctermfg = 8 })
hl('MatchParen', { ctermbg = 8, ctermfg = 7 })
hl('ColorColumn', { ctermbg = 8, ctermfg = 7 })
hl('CursorLine', { ctermbg = 'none' })
hl('CursorLineNr', { ctermbg = 'none', ctermfg = 7 })
hl('Pmenu', { ctermbg = 'none', ctermfg = 7 })
hl('FloatBorder', { ctermbg = 'none', ctermfg = 7 })
hl('DiffAdd', { ctermfg = 2 })
hl('DiffDelete', { ctermfg = 1 })
hl('vimUserCommand', { ctermfg = 1, cterm = {} })
hl('htmlTagName', { ctermfg = 1, cterm = {} })

-- plugin highlight groups
hl('TelescopeMatching', { ctermfg = 4, cterm = bold })
hl('TelescopePromptCounter', { ctermfg = 8 })
hl('TelescopePromptPrefix', { ctermbg = 'none', ctermfg = 4, cterm = bold })
hl('TelescopeSelectionCaret', { ctermbg = 8, ctermfg = 4, cterm = bold })
hl('TelescopeResultsFileIcon', { ctermfg = 4 })
hl('NvimTreeFileIcon', { ctermfg = 4 })
hl('GitSignsAdd', { ctermfg = 2 })
hl('GitSignsDelete', { ctermfg = 1 })
