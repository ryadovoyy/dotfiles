require('tokyonight').setup({
    styles = {
        comments = { italic = false },
        keywords = { italic = false }
    },
    sidebars = { 'qf', 'help', 'man' },
    lualine_bold = true,
    on_highlights = function(hl, c)
        local prompt = '#2d3149'

        -- native highlights
        hl.CursorLine = {
            bg = 'none'
        }
        hl.ColorColumn = {
            bg = 'none'
        }
        hl.FloatBorder = {
            bg = c.bg_dark,
            fg = c.bg_dark
        }

        -- make telescope borderless
        hl.TelescopeNormal = {
            bg = c.bg_dark,
            fg = c.fg_dark
        }
        hl.TelescopeBorder = {
            bg = c.bg_dark,
            fg = c.bg_dark
        }
        hl.TelescopePromptNormal = {
            bg = prompt
        }
        hl.TelescopePromptBorder = {
            bg = prompt,
            fg = prompt
        }
        hl.TelescopePromptTitle = {
            bg = c.blue,
            fg = c.black
        }
        hl.TelescopePreviewTitle = {
            bg = c.blue,
            fg = c.black
        }
        hl.TelescopeResultsTitle = {
            bg = c.bg_dark,
            fg = c.bg_dark
        }

        -- nvim-tree
        hl.NvimTreeSpecialFile = {
            fg = c.purple
        }
        hl.NvimTreeEndOfBuffer = {
            fg = c.bg_dark
        }
        hl.NvimTreeGitNew = {
            fg = c.gitSigns.add
        }
        hl.NvimTreeGitDirty = {
            fg = c.gitSigns.change
        }
        hl.NvimTreeGitDeleted = {
            fg = c.gitSigns.delete
        }

        -- custom highlights
        hl.ExtraWhitespace = {
            bg = c.error
        }
    end
})

vim.cmd.colorscheme('tokyonight-night')
