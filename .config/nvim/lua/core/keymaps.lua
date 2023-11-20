vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- tab keys
vim.keymap.set('n', '<leader>h', ':tabprev<CR>')
vim.keymap.set('n', '<leader>l', ':tabnext<CR>')

-- find files
vim.keymap.set('n', '<leader>.', ':Telescope find_files<CR>')

-- files
vim.keymap.set('n', '<leader>fs', ':w<CR>')

-- buffers
vim.keymap.set('n', '<leader>bb', ':Telescope buffers')
vim.keymap.set('n', '<leader>bk', ':bd')

-- windows
vim.keymap.set('n', '<leader>ww', '<c-w>w')
vim.keymap.set('n', '<leader>wv', '<c-w>v')
vim.keymap.set('n', '<leader>wn', '<c-w>n')

