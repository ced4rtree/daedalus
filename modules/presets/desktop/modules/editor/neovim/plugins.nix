{ inputs, config, ... }: let
  topConfig = config;
in {
  flake.modules.nvf.plugins = { config, pkgs, ... }: {
    vim = {
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
        yanky-nvim = {
          enable = true;
          setupOpts = {
            ring.storage = "sqlite";
            highlight = {
              on_put = true;
              on_yank = true;
              timer = 500;
            };
          };
        };
      };

      pluginRC.indent-blankline-init = let
        inherit (inputs.nvf.lib.nvim.dag) entryBefore;
        colors = topConfig.flake.lib.stylix.colors.withHashtag;
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
        fidget-nvim = {
          enable = true;
          setupOpts = {
            progress.display.render_limit = 5;
          };
        };
        rainbow-delimiters.enable = true;
      };
    };
  };
}
