" Set numbers on the side
set number relativenumber
" Set autocompletion
set wildmode=longest,list,full
" Set highlight while and after searching
set hlsearch
set incsearch
" Ignore case, so D is the same as d
set ic
" Set clipboard to system clipboard
set clipboard=unnamed,unnamedplus
" Tab width is 4
set tabstop=4
set autoindent
set softtabstop=4
set shiftwidth=4
" Guarantees proper file formats
set fileformat=unix
" Define leader character as ,
let mapleader = ","
" Like spellchecker for shell scripts
map <leader>s :!clear && shellcheck %<CR>
" Turns on syntax highlighting
syntax on
