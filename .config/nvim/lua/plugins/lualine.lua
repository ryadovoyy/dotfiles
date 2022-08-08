local custom_pywal = require('lualine.themes.pywal')

custom_pywal.normal.a.bg = 3
custom_pywal.normal.b.fg = 232
custom_pywal.normal.b.gui = 'bold'
custom_pywal.insert.a.bg = 3
custom_pywal.replace.a.bg = 3

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
    }
})
