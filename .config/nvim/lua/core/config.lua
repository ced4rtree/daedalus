vim.opt.termguicolors = true

vim.opt.backspace = '2'
vim.opt.showcmd = true
vim.opt.laststatus = 2
vim.opt.autowrite = true
vim.opt.cursorline = false
vim.opt.autoread = true

-- tab settings
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.shiftround = true
vim.opt.expandtab = true

-- rid of autocomments
vim.cmd([[autocmd FileType * set formatoptions-=ro]])

-- add system clipboard to neovim clipboard
vim.cmd([[set clipboard+=unnamedplus]])

-- case insensitive searching
vim.cmd([[set ignorecase]])

-- line numbers
vim.cmd([[set number number]])
