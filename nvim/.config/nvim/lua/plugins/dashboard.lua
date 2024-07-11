local ascii = require('ascii')

require('dashboard').setup({
  theme = 'hyper',
  change_to_vcs_root = true,
  config = {
    shortcut = {},
    header = ascii.art.text.neovim.dos_rebel,
    project = { limit = 3, icon = ' ' },
    mru = { limit = 5 },
  },
})
