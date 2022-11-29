local status_ok, tokyonight = pcall(require, 'tokyonight')
if not status_ok then
    return
end

tokyonight.setup({
    styles = {
        comments = { italic = false },
        keywords = { italic = false }
    },
    sidebars = {},
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

        -- treesitter-context
        hl.TreesitterContext = {
            bg = c.bg_dark
        }

        -- custom highlights
        hl.ExtraWhitespace = {
            bg = c.error
        }
    end
})

vim.cmd.colorscheme('tokyonight-night')
