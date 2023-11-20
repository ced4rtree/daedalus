vim.opt.termguicolors = true

vim.opt.backspace = '2'
vim.opt.showcmd = true
vim.opt.laststatus = 2
vim.opt.autowrite = true
vim.opt.cursorline = true
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

-- keep terminal opacity
--vim.cmd.highlight([[Normal     ctermbg=NONE guibg=NONE]])
--vim.cmd.highlight([[LineNr     ctermbg=NONE guibg=NONE]])
--vim.cmd.highlight([[SignColumn ctermbg=NONE guibg=NONE]])
