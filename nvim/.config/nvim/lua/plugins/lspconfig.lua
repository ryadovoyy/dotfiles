local set_map = require('core.util').set_map
local augroup = require('core.util').augroup
local autocmd = require('core.util').autocmd

local attach_augroup = augroup('AttachLsp', { clear = true })

autocmd('LspAttach', {
  group = attach_augroup,
  callback = function(event)
    local opts = { noremap = true, buffer = event.buf }
    local map = set_map(opts)

    map('n', '<leader>ln', vim.lsp.buf.rename, 'rename all references')
    map('n', '<leader>la', vim.lsp.buf.code_action, 'code action')
    map('n', 'K', vim.lsp.buf.hover, 'hover documentation')

    local client = vim.lsp.get_client_by_id(event.data.client_id)

    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf) then
      local highlight_augroup = augroup('HighlightLsp', { clear = false })
      local detach_augroup = augroup('DetachLsp', { clear = true })

      autocmd({ 'CursorHold', 'CursorHoldI' }, {
        group = highlight_augroup,
        buffer = event.buf,
        callback = vim.lsp.buf.document_highlight,
      })

      autocmd({ 'CursorMoved', 'CursorMovedI' }, {
        group = highlight_augroup,
        buffer = event.buf,
        callback = vim.lsp.buf.clear_references,
      })

      autocmd('LspDetach', {
        group = detach_augroup,
        callback = function(event2)
          vim.lsp.buf.clear_references()
          vim.api.nvim_clear_autocmds({ group = 'HighlightLsp', buffer = event2.buf })
        end,
      })
    end

    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
      map('n', '<leader>lh', function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
      end, 'toggle inlay hints')
    end
  end,
})

vim.diagnostic.config({
  severity_sort = true,
  float = { source = 'if_many' },
  signs = vim.g.have_nerd_font and {
    text = {
      [vim.diagnostic.severity.ERROR] = '󰅙 ',
      [vim.diagnostic.severity.WARN] = ' ',
      [vim.diagnostic.severity.INFO] = '󰋼 ',
      [vim.diagnostic.severity.HINT] = '󰌵 ',
    },
  } or {},
})

local opts = { noremap = true }
local map = set_map(opts)
map('n', '<leader>lev', vim.diagnostic.open_float, 'preview error')

local capabilities = require('blink.cmp').get_lsp_capabilities()

local servers = {
  lua_ls = {
    settings = {
      Lua = {
        completion = {
          callSnippet = 'Replace',
        },
      },
    },
  },
  gopls = {},
}

local ensure_installed = vim.tbl_keys(servers or {})
vim.list_extend(ensure_installed, {
  'stylua',
  'goimports',
  'golangci-lint',
  'hadolint',
})

require('mason-tool-installer').setup({ ensure_installed = ensure_installed })

require('mason-lspconfig').setup({
  handlers = {
    function(server_name)
      local server = servers[server_name] or {}
      server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
      require('lspconfig')[server_name].setup(server)
    end,
  },
})
