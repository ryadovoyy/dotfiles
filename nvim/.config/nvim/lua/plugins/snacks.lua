local picker = require('snacks.picker')
local explorer = require('snacks.explorer')
local lazygit = require('snacks.lazygit')
local set_map = require('core.util').set_map

require('snacks').setup({
  bigfile = {},
  dashboard = {
    example = 'doom',
  },
  explorer = {
    replace_netrw = true,
  },
  indent = {
    scope = {
      enabled = false,
    },
  },
  lazygit = {
    config = {
      gui = {
        scrollHeight = 15,
      },
      keybindings = {
        universal = {
          scrollUpMain = '<c-u>',
          scrollDownMain = '<c-d>',
        },
      },
    },
  },
  picker = {
    sources = {
      files = { hidden = true, ignored = true },
      grep = { hidden = true, ignored = true },
      explorer = {
        hidden = true,
        ignored = true,
        win = {
          list = {
            keys = {
              ['<Tab>'] = 'confirm',
              ['<CR>'] = 'select_and_next',
            },
          },
        },
      },
    },
    win = {
      input = {
        keys = {
          ['<Tab>'] = { 'confirm', mode = { 'i', 'n' } },
          ['<CR>'] = { 'select_and_next', mode = { 'i', 'n' } },
          ['<C-u>'] = { 'preview_scroll_up', mode = { 'i', 'n' } },
          ['<C-d>'] = { 'preview_scroll_down', mode = { 'i', 'n' } },
        },
      },
      preview = {
        b = {
          snacks_indent = false,
        },
      },
    },
  },
})

local opts = { noremap = true }
local map = set_map(opts)

-- find
map('n', '<leader>pf', picker.files, 'search files')

map('n', '<leader>pn', function()
  picker.files({ cwd = vim.fn.stdpath('config') })
end, 'search neovim files')

map('n', '<leader>pg', picker.git_files, 'search git files')
map('n', '<leader>po', picker.recent, 'search recent files')
map('n', '<leader>pb', picker.buffers, 'switch buffer')
map('n', '<leader>pi', picker.smart, 'smart search')
map('n', '<leader>pr', picker.grep, 'grep')
map('n', '<leader>ps', picker.lines, 'buffer search')
map('n', '<leader>pt', picker.treesitter, 'treesitter symbols')
map('n', '<leader>pq', picker.qflist, 'quickfix list')
map('n', '<leader>pu', picker.undo, 'undo history')
map('n', '<leader>pp', picker.projects, 'switch project')
map('n', '<leader>pc', picker.resume, 'continue searching')

-- help
map('n', '<leader>hd', picker.help, 'nvim docs')
map('n', '<leader>hk', picker.keymaps, 'keymaps')
map('n', '<leader>ha', picker.autocmds, 'autocmds')
map('n', '<leader>hh', picker.highlights, 'highlights')
map('n', '<leader>hm', picker.man, 'man pages')
map('n', '<leader>hs', picker.spelling, 'word spelling')

-- lsp
map('n', '<leader>ld', picker.lsp_definitions, 'go to definition')
map('n', '<leader>lh', picker.lsp_declarations, 'go to declaration')
map('n', '<leader>lr', picker.lsp_references, 'find references')
map('n', '<leader>li', picker.lsp_implementations, 'go to implementation')
map('n', '<leader>ls', picker.lsp_symbols, 'lsp symbols')
map('n', '<leader>lw', picker.lsp_workspace_symbols, 'lsp workspace symbols')

map('n', '<leader>lci', picker.lsp_incoming_calls, 'incoming calls')
map('n', '<leader>lco', picker.lsp_outgoing_calls, 'outgoing calls')
map('n', '<leader>led', picker.diagnostics, 'diagnostics')

-- explorer
map('n', '<leader>d', explorer.open, 'toggle file explorer')

-- lazygit
map('n', '<leader>gs', lazygit.open, 'open lazygit')
