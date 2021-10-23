set path+=**

set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent

set exrc
set hidden
set updatetime=300
set shortmess+=c
set regexpengine=0
set nowrap
set guicursor=
set signcolumn=yes
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
set nowritebackup
set undodir=~/.vim/undodir
set undofile

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'vim-scripts/ReplaceWithRegister'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'

Plug 'machakann/vim-highlightedyank'
" Plug 'dylanaraps/wal.vim'
" Plug 'sainnhe/gruvbox-material'
Plug 'axvr/photon.vim'
Plug 'HerringtonDarkholme/yats.vim'

call plug#end()

let mapleader = " "

" vim-commentary
autocmd FileType typescript setlocal commentstring=//\ %s
autocmd FileType typescriptreact setlocal commentstring=//\ %s

" coc
let g:coc_global_extensions = [
  \ 'coc-tsserver',
  \ 'coc-eslint', 
  \ 'coc-prettier', 
  \ ]

command! -nargs=0 Prettier :CocCommand prettier.formatFile

nmap <leader>gd <Plug>(coc-definition)
nmap <leader>gr <Plug>(coc-references)
nnoremap <silent> <leader>e :CocList diagnostics<CR>

" fzf
nnoremap <C-p> :Files<CR>
nnoremap <C-g> :GFiles<CR>
nnoremap <C-b> :Buffers<CR>
nnoremap <C-f> :Rg 

" vim-gitgutter
let g:gitgutter_map_keys = 0
nmap <leader>hp <Plug>(GitGutterPreviewHunk)

" wal
" colorscheme wal

" gruvbox-material
" let g:gruvbox_material_palette = 'mix'
" let g:gruvbox_material_transparent_background = 1
" let g:gruvbox_material_disable_italic_comment = 1
" colorscheme gruvbox-material

" photon
colorscheme photon
hi Normal guibg=none
hi Error guifg=#d75f5f
hi CursorLineNR guibg=none
hi IncSearch guibg=#767676
hi MatchParen guifg=#af87d7 gui=none
hi SpellBad guifg=#d75f5f
hi CocErrorSign guifg=#d75f5f
hi CocErrorFloat guifg=#d75f5f
hi CocWarningSign guifg=#af87d7
hi CocWarningFloat guifg=#af87d7
hi CocInfoSign guifg=#af87d7
hi CocInfoFloat guifg=#af87d7
hi GitGutterAdd guifg=#c6c6c6
hi GitGutterChange guifg=#af87d7
hi GitGutterDelete guifg=#d75f5f
hi diffAdded guifg=#c6c6c6
hi diffRemoved guifg=#d75f5f

" non-plugin remaps
" save
nnoremap <silent> <leader>s :w<CR>

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
