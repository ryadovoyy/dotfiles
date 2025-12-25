local set_map = require('core.util').set_map

require('gitsigns').setup({
  preview_config = {
    row = 1,
    col = 0,
  },
  on_attach = function(bufnr)
    local gs = require('gitsigns')
    local opts = { buffer = bufnr }
    local map = set_map(opts)

    map('n', '<leader>gb', function()
      gs.blame_line({ full = true })
    end, 'blame line')

    map('n', '<leader>ghn', function()
      gs.nav_hunk('next')
    end, 'next hunk')

    map('n', '<leader>ghp', function()
      gs.nav_hunk('prev')
    end, 'previous hunk')

    map('n', '<leader>ghv', gs.preview_hunk, 'preview hunk')
    map('n', '<leader>ghi', gs.preview_hunk_inline, 'preview hunk inline')
    map('n', '<leader>ghr', gs.reset_hunk, 'revert hunk')
  end,
})
