local custom_pywal = require('lualine.themes.pywal')
local a_colors = { fg = 232, bg = 3, gui = 'bold' }

custom_pywal.normal = {
    a = a_colors,
    b = { fg = 232, bg = 7, gui = 'bold' },
    c = { fg = 7, bg = 232 }
}
custom_pywal.insert = { a = a_colors }
custom_pywal.visual = { a = a_colors }
custom_pywal.replace = { a = a_colors }

require('lualine').setup({
    options = {
        icons_enabled = true,
        theme = custom_pywal,
        section_separators = { left = '', right = '' },
        component_separators = { left = '', right = '' },
        globalstatus = true,
        refresh = { statusline = 100 }
    },
    sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch' },
        lualine_c = { 'filename' },
        lualine_x = { 'encoding', 'fileformat', { 'filetype', colored = false } },
        lualine_y = { 'progress' },
        lualine_z = { 'location' }
    },
    extensions = { 'nvim-tree' }
})
