{ config, lib, pkgs, ... }: {
  options.daedalus.home.editor.neovim.enable = lib.mkEnableOption "neovim" // {
    default = true;
  };

  config = lib.mkIf config.daedalus.home.editor.neovim.enable {
    home.packages = with pkgs; [
      fzf
      fd
    ];

    programs.nixvim = {
      enable = true;

      opts = {
        shiftwidth = 4;
        softtabstop = 4;
        tabstop = 4;
        termguicolors = true;
        number = true;
        expandtab = true;
        scrolloff = 6;
      };

      globals = {
        autoformat = false;
        mapleader = " ";
      };

      clipboard = {
        register = "unnamedplus";
        providers.wl-copy.enable = true;
      };

      keymaps = [
        {
          action = ":w<CR>";
          key = "<leader>fs";
          mode = [ "n" "v" ];
        }
        {
          action = ":vsplit";
          key = "<leader>wv";
          mode = [ "n" "v" ];
        }
        {
          action = ":vsplit";
          key = "<leader>wn";
          mode = [ "n" "v" ];
        }
        {
          action = ":split";
          key = "<leader>wh";
          mode = [ "n" "v" ];
        }
      ];

      plugins = {
        lualine.enable = true;

        telescope = {
          enable = true;
          keymaps = {
            "<leader>ff" = "find_files";
            "<leader>fF" = "git_files";
          };
        };

        web-devicons.enable = true;

        autoclose.enable = true;

        cmp = {
          enable = true;
          autoEnableSources = true;
          settings.sources = [
            { name = "nvim_lsp"; }
            { name = "path"; }
            { name = "buffer"; }
          ];
        };

        lsp = {
          enable = true;
          servers = {
            clangd.enable = true;
            jdtls.enable = true;
            pylsp.enable = true;
            lua_ls.enable = true;
            rust_analyzer = {
              enable = true;
              installCargo = false;
              installRustc = false;
            };
          };
          keymaps = {

            lspBuf = {
              gd = "definition";
              gD = "references";
              gt = "type_definition";
              gi = "implementation";
              K = "hover";
              "<F2>" = "rename";
            };
          };
        };
      };
    };
  };
}
