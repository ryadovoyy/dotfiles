set path+=**

set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab
set smartindent
" autocmd! BufRead,BufNewFile *.ts,*.tsx,*.json setlocal ts=2 sts=2 sw=2 et

set exrc
set hidden
set updatetime=100
set shortmess+=c
set regexpengine=0
set nowrap
set guicursor=
set signcolumn=yes
set number relativenumber
set list
set listchars=tab:│\ 
set colorcolumn=80
set scrolloff=8
set incsearch
set nohlsearch
set noerrorbells
set background=dark

set noswapfile
set nobackup
set nowritebackup
set undodir=~/.vim/undodir
set undofile

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'vim-scripts/ReplaceWithRegister'

Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', {'do': 'make'}
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'lewis6991/gitsigns.nvim'
Plug 'vimwiki/vimwiki'

Plug 'machakann/vim-highlightedyank'
Plug 'dylanaraps/wal.vim'

call plug#end()

let mapleader = " "

" vim-commentary
" autocmd! FileType typescript setlocal commentstring=//\ %s
" autocmd! FileType typescriptreact setlocal commentstring=//\ %s

" nvim-lspconfig
lua << EOF
local nvim_lsp = require('lspconfig')

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = false 
  }
)

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
 
  local opts = { noremap = true, silent = true }
  buf_set_keymap('n', '<leader>gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', '<leader>gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<leader>d', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<leader>ep', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)

  if client.resolved_capabilities.document_formatting then
    vim.api.nvim_command('augroup Format')
    vim.api.nvim_command('autocmd! * <buffer>')
    vim.api.nvim_command('autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()')
    vim.api.nvim_command('augroup END')
  end
end

local servers = { 'gopls' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach
  }
end
EOF

" telescope
lua << EOF
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-j>'] = 'move_selection_next',
        ['<C-k>'] = 'move_selection_previous'
      }
    }
  }
}

require('telescope').load_extension('fzf')

local map = vim.api.nvim_set_keymap
local options = { noremap = true }

map('n', '<C-p>', '<cmd>lua require("telescope.builtin").find_files()<CR>', options)
map('n', '<C-g>', '<cmd>lua require("telescope.builtin").git_files()<CR>', options)
map('n', '<C-b>', '<cmd>lua require("telescope.builtin").buffers()<CR>', options)
map('n', '<C-f>', '<cmd>lua require("telescope.builtin").live_grep()<CR>', options)
map('n', '<leader>fd', '<cmd>lua require("telescope.builtin").lsp_workspace_diagnostics()<CR>', options)
EOF

" nvim-treesitter
lua << EOF
require'nvim-treesitter.configs'.setup {
  highlight = { enable = true },
  ensure_installed = {
    'bash',
    'dockerfile',
    'go',
    'gomod',
    'graphql',
    'html',
    'json',
    'lua',
    'regex',
    'toml',
    'yaml'
  }
}
EOF

" gitsigns
lua << EOF
require('gitsigns').setup {
  preview_config = {
    border = 'none',
    style = 'minimal',
    relative = 'cursor',
    row = 1,
    col = 0
  }
}
EOF

" vimwiki
let g:vimwiki_list = [{'path': '~/cloud/vimwiki/',
  \ 'syntax': 'markdown', 'ext': '.md'}]

" wal
colorscheme wal
hi Identifier cterm=none
hi StatusLine ctermfg=232
hi Pmenu ctermbg=0
hi DiffAdd ctermfg=107
hi DiffDelete ctermfg=167
hi ColorColumn ctermbg=0 ctermfg=7
hi diffAdded ctermfg=107
hi diffRemoved ctermfg=167
hi vimUserCommand cterm=none
hi htmlTagName cterm=none

" non-plugin settings and remaps
" netrw
let g:netrw_keepdir = 0
nnoremap <leader>n :Ex<CR>

" save and quit
nnoremap <leader>s :w<CR><C-l>
nnoremap <silent> <leader>q :q<CR>
inoremap jk <Esc>

" clipboard
nnoremap <leader>y "+y
nnoremap <leader>p "+p

" toggle between buffers
nnoremap <leader><leader> <C-^>

" make Y behave like the rest of the capital letters
nnoremap Y y$

" keep it centered
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz

" quickfix list
nnoremap <silent> <C-j> :cnext<CR>zz
nnoremap <silent> <C-k> :cprev<CR>zz
nnoremap <silent> <leader>cc :ccl<CR>

" location list
nnoremap <silent> <leader>j :lnext<CR>zz
nnoremap <silent> <leader>k :lprev<CR>zz
nnoremap <silent> <leader>lc :lcl<CR>

" move text
" nnoremap <silent> <leader>j :m .+1<CR>==
" nnoremap <silent> <leader>k :m .-2<CR>==
