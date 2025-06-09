{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [
    fzf
    fd
  ];
  
  programs.nixvim = {
    enable = true;

    colorschemes.catppuccin = {
      enable = true;
      flavor = "frappe";
    };

    plugins = {
      lualine.enable = true;

      telescope = {
        enable = true;
        keymaps = {
          "<leader>ff" = "find_files";
          "<leader>fF" = "git_files";
        };
      };
    }

    options = {
      shiftwidth = 4;
      softtabstop = 4;
      tabstop = 4;
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
      lsp = {
        enable = true;
        servers = {
          "*" = {
            settings = {
              capabilities = {
                textDocument = {
                  semanticTokens = {
                    multilineTokenSupport = true;
                  };
                };
              };
              root_markers = [
                ".git"
              ];
            };
          };
          clangd = {
            enable = true;
            settings = {
              cmd = [
                "clangd"
                "--background-index"
              ];
              filetypes = [
                "c"
                "cpp"
              ];
              root_markers = [
                "compile_commands.json"
                "compile_flags.txt"
              ];
            };
          };
          jdtls = {
            enable = true;
            settings = {
              filetypes = [ "java" ];
              root_markers = [ "build.gradle" "pom.xml" ];
            };
          };
          pylsp = {
            enable = true;
            filetypes = [ "py" ];
            root_markers = [ "requirements.txt" "environment.yml" "dev-environment.yml" ];
          };
          rust_analyzer = {
            enable = true;
            filetypes = [ "rs" ];
            root_markers = [ "cargo.toml" ];
          };
          lua_ls = {
            enable = true;
          };
        };
        keymaps = [
          {
            key = "gd";
            lspBufAction = "definition";
          }
          {
            key = "gD";
            lspBufAction = "references";
          }
          {
            key = "gt";
            lspBufAction = "type_definition";
          }
          {
            key = "gi";
            lspBufAction = "implementation";
          }
          {
            key = "K";
            lspBufAction = "hover";
          }
          {
            action = lib.nixvim.mkRaw "function() vim.diagnostic.jump({ count=-1, float=true }) end";
            key = "<leader>k";
          }
          {
            action = lib.nixvim.mkRaw "function() vim.diagnostic.jump({ count=1, float=true }) end";
            key = "<leader>j";
          }
          {
            action = "<CMD>LspStop<Enter>";
            key = "<leader>lx";
          }
          {
            action = "<CMD>LspStart<Enter>";
            key = "<leader>ls";
          }
          {
            action = "<CMD>LspRestart<Enter>";
            key = "<leader>lr";
          }
          {
            action = lib.nixvim.mkRaw "require('telescope.builtin').lsp_definitions";
            key = "gd";
          }
          {
            action = "<CMD>Lspsaga hover_doc<Enter>";
            key = "K";
          }
        ];
      };
    };
  };
}
