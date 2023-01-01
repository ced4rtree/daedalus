" Disable vi compatibility
set nocompatible

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

" Sets the mouse to be disabled entirely
set mouse=

" Guarantees proper file formats
set fileformat=unix

" Define leader character as ,
let mapleader = " "

" Like spellchecker for shell scripts
map <leader>s :!clear && shellcheck %<CR>

" Turns on syntax highlighting
syntax on

" Visualizes tabs in similar way to how vscode does
set listchars=tab:\ \ \|
set invlist

" Auto indents scope operators
inoremap {<CR> {<CR><CR>}<up><tab>

" Completes System.out.println(); for you. Useful in Java. 
inoremap sout System.out.println();<left><left>

" Completes /* for you
inoremap /*<CR> /*<CR><CR>*/<up>

" Creates a C++ skeleton
map <leader>C <ins>#include<space><iostream><CR><CR>int<space>main()<space>{<CR><CR><CR>return<space>0;<up><up>

" Creates a C skeleton
map <leader>c <ins>#include<space><stdio.h><CR>#include<space><stdlib.h><CR><CR>int<space>main()<space>{<CR><CR><CR>return<space>0;<up><up>

" Creates a Java skeleton
map <leader>j <ins>public<space>class<space><esc>"%p<left><left><left><left>xxxxxA<space>{<CR>public<space>static<space>void<space>main((String[]<space>args)<space>{<CR><tab>

" Moves to a different tab
map <leader>h :tabprev<CR>
map <leader>l :tabnext<CR>
map <leader>H :tabfirst<CR>
map <leader>L :tablast<CR>

" Builds C files
map <C-c> :w<CR>:!gcc<space>%<space>-o<space>%<

" Builds C++ files
map <C-p> :w<CR>:!g++<space>%<space>-o<space>%<

" Builds Java files
map <C-j> :w<CR>:!javac<space>%

" Puts a tab before every highlighted line
vmap <tab> :'<,'>norm<space>I<tab><CR>
