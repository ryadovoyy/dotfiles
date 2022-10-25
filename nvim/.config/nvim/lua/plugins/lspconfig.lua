local lsp = require('lspconfig')
local set_map = require('core.util').set_map
local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

vim.diagnostic.config({ virtual_text = false })

local function on_attach(client, bufnr)
    local opts = { noremap = true, silent = true, buffer = bufnr }
    local map = set_map(opts)

    map('n', '<leader>ln', vim.lsp.buf.rename, 'rename all references')
    map('n', '<leader>lev', vim.diagnostic.open_float, 'preview error')

    if client.server_capabilities.documentFormattingProvider then
        local format_augroup = augroup('Format', { clear = true })
        autocmd('BufWritePre', {
            group = format_augroup,
            buffer = bufnr,
            callback = function()
                vim.lsp.buf.formatting_sync()
            end
        })
    end
end

local servers = { 'gopls' }
for _, server in ipairs(servers) do
    lsp[server].setup({
        on_attach = on_attach
    })
end
