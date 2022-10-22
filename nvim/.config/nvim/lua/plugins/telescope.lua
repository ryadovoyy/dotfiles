local tsb = require('telescope.builtin')
local set_map = require('core.util').set_map

require('telescope').setup({
    defaults = {
        prompt_prefix = '   ',
        selection_caret = '  ',
        entry_prefix = '  ',
        sorting_strategy = 'ascending',
        layout_config = {
            horizontal = {
                prompt_position = 'top',
                preview_width = 0.55
            }
        },
        file_ignore_patterns = { '^.git/' },
        mappings = {
            i = {
                ['<C-j>'] = 'move_selection_next',
                ['<C-k>'] = 'move_selection_previous',
                ['<Tab>'] = 'select_default',
                ['<CR>'] = 'toggle_selection'
            }
        }
    },
    pickers = {
        find_files = {
            hidden = true,
            no_ignore = true,
            follow = true
        },
        live_grep = {
            additional_args = function(opts)
                return { '--hidden' }
            end
        }
    }
})

local opts = { noremap = true }
local map = set_map(opts)

-- search
map('n', '<leader>pp', tsb.resume)
map('n', '<leader>pf', tsb.find_files)
map('n', '<leader>ph', tsb.oldfiles)
map('n', '<leader>pb', tsb.buffers)
map('n', '<leader>pr', tsb.live_grep)
map('n', '<leader>ps', tsb.current_buffer_fuzzy_find)
map('n', '<leader>pt', tsb.treesitter)

-- help
map('n', '<leader>hd', tsb.help_tags)
map('n', '<leader>hv', tsb.vim_options)
map('n', '<leader>hk', tsb.keymaps)
map('n', '<leader>ho', tsb.highlights)
map('n', '<leader>hm', tsb.man_pages)

-- lsp
map('n', '<leader>ld', tsb.lsp_definitions)
map('n', '<leader>lr', tsb.lsp_references)
map('n', '<leader>led', tsb.diagnostics)
