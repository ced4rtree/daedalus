require('nvim_comment').setup()
vim.keymap.set('n', '<c-/>', ':CommentToggle<CR>')
vim.keymap.set('v', '<c-/>', ':\'<,\'>CommentToggle<CR>')
