local status_ok, telescope = pcall(require, 'telescope')
if not status_ok then
    return
end

local tsb = require('telescope.builtin')
local set_map = require('core.util').set_map

telescope.setup({
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
    },
    extensions = {
        project = {
            hidden_files = true,
            sync_with_nvim_tree = true
        }
    }
})

telescope.load_extension('fzf')
telescope.load_extension('project')

local tspr = telescope.extensions.project.project
local opts = { noremap = true }
local map = set_map(opts)

-- files
map('n', '<leader>pc', tsb.resume, 'continue searching')
map('n', '<leader>pf', tsb.find_files, 'open file')
map('n', '<leader>po', tsb.oldfiles, 'search old files')
map('n', '<leader>pb', tsb.buffers, 'switch buffer')
map('n', '<leader>pr', tsb.live_grep, 'ripgrep')
map('n', '<leader>ps', tsb.current_buffer_fuzzy_find, 'buffer search')
map('n', '<leader>pt', tsb.treesitter, 'treesitter symbols')
map('n', '<leader>pp', tspr, 'switch project')

-- help
map('n', '<leader>hd', tsb.help_tags, 'nvim docs')
map('n', '<leader>ho', tsb.vim_options, 'nvim options')
map('n', '<leader>hk', tsb.keymaps, 'keymaps')
map('n', '<leader>ha', tsb.autocommands, 'autocmd')
map('n', '<leader>hh', tsb.highlights, 'highlights')
map('n', '<leader>hm', tsb.man_pages, 'man pages')

-- lsp
map('n', '<leader>ld', tsb.lsp_definitions, 'find definitions')
map('n', '<leader>lr', tsb.lsp_references, 'find references')
map('n', '<leader>led', tsb.diagnostics, 'diagnostics')
