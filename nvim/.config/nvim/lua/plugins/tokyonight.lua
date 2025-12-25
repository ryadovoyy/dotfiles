require('tokyonight').setup({
  style = 'night',
  styles = {
    keywords = { italic = false },
  },
  lualine_bold = true,
  on_colors = function(c)
    local bg_dark = '#13141c'
    local bg_statusline = '#171822'

    c.bg_statusline = bg_statusline
    c.bg_dark = bg_dark
    c.bg_float = bg_dark
    c.bg_popup = bg_dark
    c.bg_sidebar = bg_dark
    c.border = bg_dark
  end,
  on_highlights = function(hl, c)
    -- native highlights
    hl.CursorLine = {
      bg = 'none',
    }
    hl.CursorLineNr = {
      fg = c.fg_sidebar,
    }

    -- snacks
    hl.SnacksWinSeparator = {
      fg = c.bg_dark,
      bg = c.bg_dark,
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
