require('nvim-treesitter.configs').setup({
    highlight = { enable = true },
    ensure_installed = {
        'bash',
        'c',
        'cmake',
        'cpp',
        'dockerfile',
        'go',
        'gomod',
        'graphql',
        'help',
        'html',
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
