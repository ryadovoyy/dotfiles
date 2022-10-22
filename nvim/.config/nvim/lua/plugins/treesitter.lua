require('nvim-treesitter.configs').setup({
    highlight = { enable = true },
    indent = { enable = true },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = '@class.inner'
            }
        }
    },
    ensure_installed = {
        'bash',
        'c',
        'cmake',
        'comment',
        'cpp',
        'dockerfile',
        'go',
        'gomod',
        'graphql',
        'help',
        'html',
        'http',
        'json',
        'lua',
        'make',
        'markdown',
        'python',
        'regex',
        'rust',
        'toml',
        'yaml'
    }
})
