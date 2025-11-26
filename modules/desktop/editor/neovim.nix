{
  flake.modules.homeManager.neovim = { pkgs, inputs, lib, config, ... }: {
    imports = [
      inputs.nvf.homeManagerModules.default
    ];

    stylix.targets.nvf.enable = false;

    programs.nvf = {
      enable = true;

      settings.vim = {
        theme = {
          enable = true;
          name = "base16-pro-max";
          transparent = true;
          base16-colors = {
            inherit (config.lib.stylix.colors.withHashtag)
            base00
            base01
            base02
            base03
            base04
            base05
            base06
            base07
            base08
            base09
            base0A
            base0B
            base0C
            base0D
            base0E
            base0F
            ;
          };
        };

        options.scrolloff = 6;

        keymaps = [
          # file bindings
          {
            key = "<leader>fs";
            mode = [ "n" "v" ];
            action = ":w<CR>";
            desc = "Save file";
          }
          {
            key = "<leader>.";
            mode = [ "n" "v" ];
            action = ":Telescope file_browser path=%:p:h select_buffer=true<CR>";
            desc = "Find files in cwd [Telescope]";
          }
          {
            key = "<leader>pf";
            mode = [ "n" "v" ];
            action = ":Telescope find_files<CR>";
            desc = "Find file [Telescope]";
          }

          # buffer bindings
          {
            key = "<leader>bp";
            mode = [ "n" "v" ];
            action = ":bprevious<CR>";
            desc = "Go to previous buffer";
          }
          {
            key = "<leader>bn";
            mode = [ "n" "v" ];
            action = ":bnext<CR>";
            desc = "Go to next buffer";
          }

          # git
          {
            key = "<leader>gg";
            mode = [ "n" "v" ];
            action = ":Neogit<CR>";
            desc = "Git Status [Neogit]";
          }

          # window bindings
          {
            key = "<leader>ww";
            mode = [ "n" "v" ];
            action = "<C-w>w";
            desc = "Switch window focus";
          }
          {
            key = "<leader>wv";
            mode = [ "n" "v" ];
            action = "<C-w>v";
            desc = "Open vertical split";
          }
          {
            key = "<leader>wn";
            mode = [ "n" "v" ];
            action = "<C-w>n";
            desc = "Open horizontal split";
          }
          {
            key = "<leader>wc";
            mode = [ "n" "v" ];
            action = "<C-w>c";
            desc = "Close current window";
          }
          {
            key = "<leader>w1";
            mode = [ "n" "v" ];
            action = "<C-w>o";
            desc = "Close other windows";
          }
          {
            key = "<leader>w0";
            mode = [ "n" "v" ];
            action = "<C-w>o";
            desc = "Close current window";
          }

          # miscellaneous
          {
            key = "<leader>ot";
            mode = [ "n" "v" ];
            action = ":ToggleTerm<CR>";
            desc = "Toggle terminal";
          }
          {
            key = "<leader>oe";
            mode = [ "n" "v" ];
            action = ":Explore<CR>";
            desc = "Open NetRW (File Explorer)";
          }
        ];

        autopairs.nvim-autopairs.enable = true;
        dashboard.alpha = {
          enable = true;
          theme = "theta";
        };
        debugger.nvim-dap.ui.enable = true;
        comments.comment-nvim.enable = true;
        statusline.lualine = {
          enable = true;
          theme = "base16";
        };
        telescope = {
          enable = true;
          extensions = [
            {
              name = "file_browser";
              packages = [ pkgs.vimPlugins.telescope-file-browser-nvim ];
            }
          ];
        };

        terminal.toggleterm = {
          enable = true;
        };

        clipboard = {
          registers = "unnamedplus";
          providers.wl-copy.enable = true;
          enable = true;
        };

        treesitter = {
          enable = true;
          context.enable = true;
          fold = true;
        };

        autocomplete.blink-cmp = {
          enable = true;
          friendly-snippets.enable = true;
        };

        git = {
          neogit.enable = true;
          gitsigns = {
            enable = true;
            setupOpts = {
              current_line_blame = true;
            };
          };
        };

        languages = {
          enableDAP = true;
          enableTreesitter = true;
          
          nix.enable = true;
          rust.enable = true;
          java.enable = true;
          python.enable = true;
          clang.enable = true;
        };

        lsp = {
          enable = true;
          inlayHints.enable = true;
        };

        presence.neocord = {
          enable = true;
          setupOpts.enable_line_number = true;
        };

        ui = {
          colorful-menu-nvim.enable = true;
          colorizer.enable = true;
          noice.enable = true;
        };

        binds.whichKey.enable = true;

        utility = {
          direnv.enable = true;
          sleuth.enable = true;
        };

        pluginRC.indent-blankline-init = let
          inherit (inputs.nvf.lib.nvim.lua) toLuaObject;
          inherit (inputs.nvf.lib.nvim.dag) entryBefore;
          colors = config.lib.stylix.colors.withHashtag;
          cfg = config.programs.nvf.settings.vim.visuals.indent-blankline;
        in entryBefore ["indent-blankline"] ''
          local hooks = require "ibl.hooks"
          -- create the highlight groups in the highlight setup hook, so they are reset
          -- every time the colorscheme changes
          hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
              vim.api.nvim_set_hl(0, "RainbowRed", { fg = "${colors.red}" })
              vim.api.nvim_set_hl(0, "RainbowYellow", { fg = "${colors.yellow}" })
              vim.api.nvim_set_hl(0, "RainbowBlue", { fg = "${colors.blue}" })
              vim.api.nvim_set_hl(0, "RainbowOrange", { fg = "${colors.orange}" })
              vim.api.nvim_set_hl(0, "RainbowGreen", { fg = "${colors.green}" })
              vim.api.nvim_set_hl(0, "RainbowViolet", { fg = "${colors.magenta}" })
              vim.api.nvim_set_hl(0, "RainbowCyan", { fg = "${colors.cyan}" })
          end)
        '';
        visuals = {
          indent-blankline = {
            enable = true;
            setupOpts = {
              indent = {
                highlight = [
                  "RainbowRed"
                  "RainbowYellow"
                  "RainbowBlue"
                  "RainbowOrange"
                  "RainbowGreen"
                  "RainbowViolet"
                  "RainbowCyan"
                ];
              };
            };
          };
          fidget-nvim.enable = true;
          rainbow-delimiters.enable = true;
        };
      };
    };
  };
}
