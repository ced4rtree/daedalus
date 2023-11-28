local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    -- tree-sitter
    use {
        'nvim-treesitter/nvim-treesitter',
        run = function()
            local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
            ts_update()
        end,
    }

    -- lsp
    use {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v3.x',
        requires = {
            -- LSP Support
            {'neovim/nvim-lspconfig'},
            {'williamboman/mason.nvim'},
            {'williamboman/mason-lspconfig.nvim'},

            -- Autocompletion
            {'hrsh7th/nvim-cmp'},
            {'hrsh7th/cmp-buffer'},
            {'hrsh7th/cmp-path'},
            {'saadparwaiz1/cmp_luasnip'},
            {'hrsh7th/cmp-nvim-lsp'},
            {'hrsh7th/cmp-nvim-lua'},

            -- Snippets
            {'L3MON4D3/LuaSnip'},
            {'rafamadriz/friendly-snippets'},
        }
    }

    -- nvim-tree
    use {
        'nvim-tree/nvim-tree.lua',
        requires = { 'nvim-tree/nvim-web-devicons' },
        config = function()
            vim.keymap.set('n', '<leader>t', ':NvimTreeToggle<CR>')
        end
    }

    -- dashboard
    use {
        'nvimdev/dashboard-nvim',
        event = 'VimEnter',
        config = function()
            require("dashboard").setup {
                config = {
                    header = {
                        " ▄▄        ▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄               ▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄       ▄▄ ",
                        "▐░░▌      ▐░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░▌             ▐░▌▐░░░░░░░░░░░▌▐░░▌     ▐░░▌",
                        "▐░▌░▌     ▐░▌▐░█▀▀▀▀▀▀▀▀▀ ▐░█▀▀▀▀▀▀▀█░▌ ▐░▌           ▐░▌  ▀▀▀▀█░█▀▀▀▀ ▐░▌░▌   ▐░▐░▌",
                        "▐░▌▐░▌    ▐░▌▐░▌          ▐░▌       ▐░▌  ▐░▌         ▐░▌       ▐░▌     ▐░▌▐░▌ ▐░▌▐░▌",
                        "▐░▌ ▐░▌   ▐░▌▐░█▄▄▄▄▄▄▄▄▄ ▐░▌       ▐░▌   ▐░▌       ▐░▌        ▐░▌     ▐░▌ ▐░▐░▌ ▐░▌",
                        "▐░▌  ▐░▌  ▐░▌▐░░░░░░░░░░░▌▐░▌       ▐░▌    ▐░▌     ▐░▌         ▐░▌     ▐░▌  ▐░▌  ▐░▌",
                        "▐░▌   ▐░▌ ▐░▌▐░█▀▀▀▀▀▀▀▀▀ ▐░▌       ▐░▌     ▐░▌   ▐░▌          ▐░▌     ▐░▌   ▀   ▐░▌",
                        "▐░▌    ▐░▌▐░▌▐░▌          ▐░▌       ▐░▌      ▐░▌ ▐░▌           ▐░▌     ▐░▌       ▐░▌",
                        "▐░▌     ▐░▐░▌▐░█▄▄▄▄▄▄▄▄▄ ▐░█▄▄▄▄▄▄▄█░▌       ▐░▐░▌        ▄▄▄▄█░█▄▄▄▄ ▐░▌       ▐░▌",
                        "▐░▌      ▐░░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌        ▐░▌        ▐░░░░░░░░░░░▌▐░▌       ▐░▌",
                        " ▀        ▀▀  ▀▀▀▀▀▀▀▀▀▀▀  ▀▀▀▀▀▀▀▀▀▀▀          ▀          ▀▀▀▀▀▀▀▀▀▀▀  ▀         ▀ ",
                        "",
                        "",
                        "",
                        "",
                        ""
                    }
                }
            }
        end,
        requires = {'tree-nvim/nvim-web-devicons'}
    }

    -- telescope, a completion framework for neovim
    use {
        "nvim-telescope/telescope.nvim",
        tag = "0.1.x",
        requires = { "nvim-lua/plenary.nvim" },
    }

    -- catppuccin-frappe theme
    use {
        "catppuccin/nvim",
        as = "catppuccin",
        config = function()
            vim.cmd.colorscheme "catppuccin-frappe"
        end,
    }

    -- automatically pair braces
    use 'jiangmiao/auto-pairs'

    -- rainbow-delimiters
    use 'HiPhish/rainbow-delimiters.nvim'

    -- show colors for hex values
    use 'norcalli/nvim-colorizer.lua'

    -- functions to comment out lines
    use 'terrortylor/nvim-comment'

    -- which-key
    -- show options for keypresses in the bottom
    use {
        "folke/which-key.nvim",
        config = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300
        end
    }

    -- lualine, a neat little modeline
    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'nvim-tree/nvim-web-devicons', opt = true },
    }

    -- neogit, magit clone for neovim
    use {
        'NeogitOrg/neogit',
        requires = {
            { 'nvim-lua/plenary.nvim', opt = false, },
            { 'nvim-telescope/telescope.nvim', opt = true, },
            { 'sindrets/diffview.nvim', opt = true },
            { 'ibhagwan/fzf-lua', opt = true }
        },
    }

    -- haskell support
    use {
        'neovimhaskell/haskell-vim',

       setup = function ()
            vim.cmd([[syntax on]])
            vim.cmd([[filetype plugin indent on]])
        end,

        config = function ()
            vim.cmd([[let g:haskell_enable_quantification = 1]])   -- to enable highlighting of `forall`
            vim.cmd([[let g:haskell_enable_recursivedo = 1]])      -- to enable highlighting of `mdo` and `rec`
            vim.cmd([[let g:haskell_enable_arrowsyntax = 1]])      -- to enable highlighting of `proc`
            vim.cmd([[let g:haskell_enable_pattern_synonyms = 1]]) -- to enable highlighting of `pattern`
            vim.cmd([[let g:haskell_enable_typeroles = 1]])        -- to enable highlighting of type roles
            vim.cmd([[let g:haskell_enable_static_pointers = 1]])  -- to enable highlighting of `static`
            vim.cmd([[let g:haskell_backpack = 1]])                -- to enable highlighting of backpack keywords
        end,
    }

    if packer_bootstrap then
        require('packer').sync()
    end

end)
