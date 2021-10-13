set path+=**

set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent

set exrc
set hidden
set nowrap
set guicursor=
" set signcolumn=yes
set number relativenumber
set colorcolumn=80
set scrolloff=8
set incsearch
set nohlsearch
set noerrorbells
set background=dark
if has('termguicolors')
  set termguicolors
endif

set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'vim-scripts/ReplaceWithRegister'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'machakann/vim-highlightedyank'
" Plug 'dylanaraps/wal.vim'
Plug 'sainnhe/gruvbox-material'

call plug#end()

let mapleader = " "

" vim-commentary

" coc
nnoremap <leader>gd <Plug>(coc-definition)
nnoremap <leader>gr <Plug>(coc-references)

" fzf
nnoremap <C-p> :Files<CR>
nnoremap <C-g> :GFiles<CR>
nnoremap <C-f> :Rg! 

" wal
" colorscheme wal

" gruvbox-material
let g:gruvbox_material_palette = 'mix'
let g:gruvbox_material_disable_italic_comment = 1
colorscheme gruvbox-material
highlight Normal ctermbg=none guibg=none
highlight CursorLineNR ctermbg=none guibg=none

" non-plugin remaps
" toggle between buffers
nnoremap <leader><leader> <C-^>

" make Y behave like the rest of the capital letters
nnoremap Y y$

" keep it centered
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap <C-j> :cnext<CR>zz
nnoremap <C-k> :cprev<CR>zz

" move text
nnoremap <leader>j :m .+1<CR>==
nnoremap <leader>k :m .-2<CR>==
