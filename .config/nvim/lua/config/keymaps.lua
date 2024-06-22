-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set('n', '<leader>fs', ':w<CR>')
vim.keymap.set('v', '<leader>fs', ':w<CR>')

vim.keymap.set('n', '<leader>.', '<leader>ff')
vim.keymap.set('v', '<leader>.', '<leader>ff')

vim.keymap.set('n', '<leader>,', '<leader>fF')
vim.keymap.set('v', '<leader>,', '<leader>fF')

vim.keymap.set('n', '<leader>wv', '<leader>w|')
vim.keymap.set('n', '<leader>wn', '<leader>w-')
vim.keymap.set('v', '<leader>wv', '<leader>w|')
vim.keymap.set('v', '<leader>wn', '<leader>w-')
