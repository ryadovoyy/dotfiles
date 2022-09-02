local M = {}

function M.set_map(opts)
    return function(mode, lhs, rhs)
        vim.keymap.set(mode, lhs, rhs, opts)
    end
end

M.augroup = vim.api.nvim_create_augroup
M.autocmd = vim.api.nvim_create_autocmd

return M
