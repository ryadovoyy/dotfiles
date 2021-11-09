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

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/fzf.vim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-lua/plenary.nvim'
Plug 'lewis6991/gitsigns.nvim'
Plug 'vimwiki/vimwiki'

Plug 'machakann/vim-highlightedyank'
Plug 'dylanaraps/wal.vim'

call plug#end()

let mapleader = " "

" vim-commentary
" autocmd! FileType typescript setlocal commentstring=//\ %s
" autocmd! FileType typescriptreact setlocal commentstring=//\ %s

" coc
" let g:coc_global_extensions = [
"   \ 'coc-tsserver',
"   \ 'coc-eslint', 
"   \ 'coc-prettier', 
"   \ ]

" command! -nargs=0 Prettier :CocCommand prettier.formatFile

nmap <leader>gd <Plug>(coc-definition)
nmap <leader>gr <Plug>(coc-references)
nnoremap <silent> <leader>e :CocList diagnostics<CR>

" fzf
nnoremap <C-p> :Files<CR>
nnoremap <C-g> :GFiles<CR>
nnoremap <C-b> :Buffers<CR>
nnoremap <C-f> :Rg 

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
    border = 'rounded',
    style = 'minimal',
    relative = 'cursor',
    row = 0,
    col = 1
  }
}
EOF

" vimwiki
let g:vimwiki_list = [{'path': '~/cloud/vimwiki/',
  \ 'syntax': 'markdown', 'ext': '.md'}]

" wal
colorscheme wal
hi StatusLine ctermfg=232
hi CursorLine ctermbg=0 ctermfg=7 cterm=none
hi Pmenu ctermbg=0
hi DiffAdd ctermfg=107
hi DiffDelete ctermfg=167
hi ColorColumn ctermbg=0
hi CocErrorSign ctermfg=167
hi CocErrorFloat ctermfg=167
hi CocWarningSign ctermfg=179
hi CocWarningFloat ctermfg=179
hi CocInfoSign ctermfg=179
hi CocInfoFloat ctermfg=179
hi diffAdded ctermfg=107
hi diffRemoved ctermfg=167

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
nnoremap <silent> <C-j> :cnext<CR>zz
nnoremap <silent> <C-k> :cprev<CR>zz

" move text
nnoremap <silent> <leader>j :m .+1<CR>==
nnoremap <silent> <leader>k :m .-2<CR>==
