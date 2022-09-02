local custom_pywal = require('lualine.themes.pywal')

-- -- turn #rrggbb -> { red, green, blue }
-- local function rgb_str2num(rgb_color_str)
--     if rgb_color_str:find('#') == 1 then
--         rgb_color_str = rgb_color_str:sub(2, #rgb_color_str)
--     end
--     local red = tonumber(rgb_color_str:sub(1, 2), 16)
--     local green = tonumber(rgb_color_str:sub(3, 4), 16)
--     local blue = tonumber(rgb_color_str:sub(5, 6), 16)
--     return { red = red, green = green, blue = blue }
-- end

-- -- turn { red, green, blue } -> #rrggbb
-- local function rgb_num2str(rgb_color_num)
--     local rgb_color_str = string.format('#%02x%02x%02x', rgb_color_num.red, rgb_color_num.green, rgb_color_num.blue)
--     return rgb_color_str
-- end

-- -- clamp the val between left and right
-- local function clamp(val, left, right)
--     if val > right then
--         return right
--     end
--     if val < left then
--         return left
--     end
--     return val
-- end

-- -- change brightness of rgb_color by percentage
-- local function brightness_modifier(rgb_color, parcentage)
--     local color = rgb_str2num(rgb_color)
--     color.red = clamp(color.red + (color.red * parcentage / 100), 0, 255)
--     color.green = clamp(color.green + (color.green * parcentage / 100), 0, 255)
--     color.blue = clamp(color.blue + (color.blue * parcentage / 100), 0, 255)
--     return rgb_num2str(color)
-- end

-- local b_bg_color = brightness_modifier(custom_pywal.inactive.b.bg, 150)
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
