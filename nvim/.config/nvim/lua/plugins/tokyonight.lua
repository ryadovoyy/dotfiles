require('tokyonight').setup({
  style = 'night',
  styles = {
    keywords = { italic = false },
  },
  lualine_bold = true,
  on_colors = function(c)
    local bg = '#13141c'

    c.bg_statusline = '#171822'
    c.bg_dark = bg
    c.bg_float = bg
    c.bg_popup = bg
    c.bg_sidebar = bg
    c.border = bg
  end,
  on_highlights = function(hl, c)
    local prompt = '#2d3149'

    -- native highlights
    hl.CursorLine = {
      bg = 'none',
    }
    hl.CursorLineNr = {
      fg = c.fg_sidebar,
    }
    hl.ColorColumn = {
      bg = 'none',
    }

    -- make telescope borderless
    hl.TelescopeNormal = {
      bg = c.bg_dark,
      fg = c.fg_dark,
    }
    hl.TelescopeBorder = {
      bg = c.bg_dark,
      fg = c.bg_dark,
    }
    hl.TelescopePromptNormal = {
      bg = prompt,
    }
    hl.TelescopePromptBorder = {
      bg = prompt,
      fg = prompt,
    }
    hl.TelescopePromptTitle = {
      bg = c.blue,
      fg = c.black,
    }
    hl.TelescopePreviewTitle = {
      bg = c.blue,
      fg = c.black,
    }
    hl.TelescopeResultsTitle = {
      bg = c.bg_dark,
      fg = c.bg_dark,
    }

    -- nvim-tree
    hl.NvimTreeNormal = {
      fg = c.fg_sidebar,
      bg = c.bg_dark,
    }
    hl.NvimTreeEndOfBuffer = {
      fg = c.bg_dark,
    }
    hl.NvimTreeWinSeparator = {
      fg = c.bg_dark,
      bg = c.bg_dark,
    }
    hl.NvimTreeSpecialFile = {
      fg = c.purple,
    }
    hl.NvimTreeGitNew = {
      fg = c.git.add,
    }
    hl.NvimTreeGitDirty = {
      fg = c.git.change,
    }
    hl.NvimTreeGitDeleted = {
      fg = c.git.delete,
    }

    -- treesitter-context
    hl.TreesitterContext = {
      bg = c.bg_dark,
    }

    -- custom highlights
    hl.ExtraWhitespace = {
      bg = c.error,
    }
  end,
})

vim.cmd.colorscheme('tokyonight-night')
