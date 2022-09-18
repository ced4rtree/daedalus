" Numbers on the side
set number relativenumber
" Color scheme
set bg=light
" Auto completion
set wildmode=longest,list,full
" Open a split window to the right instead of below
set splitbelow splitright
" Highlight my searches
set hlsearch
" Highlight my searches as I'm searching
set incsearch
" Don't care about capitalization when searching
set ic
" Set default clipboard to system clipboard
set clipboard=unnamedplus

"tabs
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Guarantee file format is correct
set fileformat=unix

" Color various words by default, e.g. functions names can be orange
syntax on

" Basically spellcheck for shell scripts
map <leader>s :!clear && shellcheck %<CR>
