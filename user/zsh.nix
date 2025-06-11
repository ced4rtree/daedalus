{ config, pkgs, lib, ... }: let
  user = "cedar";
  homeManagerSessionVars = "/etc/profiles/per-user/${user}/etc/profile.d/hm-session-vars.sh";
in {
  home.packages = with pkgs; [
    pokemon-colorscripts-mac
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    autocd = true;

    dotDir = ".config/zsh";

    shellAliases = {
      l = "ls --color=auto";
      ls = "l";
      ll = "ls -l";
      la = "ls -a";
      lh = "ls -h";
      llh = "ls -lh";
      lla = "ls -la";
      llha = "ls -lha";
    };

    history = {
      size = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
    };

    initContent = ''
      autoload -U compinit
      zstyle ':completion:*' menu select
      zmodload zsh/complist
      compinit
      _comp_options+=(globdots)

      # enable conda
      [ -f ~/.conda/etc/profile.d/conda.sh ] && command -v conda>/dev/null && source ~/.conda/etc/profile.d/conda.sh

      pokemon-colorscripts -r | awk 'NR>1 { print $0 }'
    '';

    sessionVariables = {
      PATH = "$PATH:$HOME/.local/bin/";
    };
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      format = 
        "[](#e78284)"
        + "$python"
        + "$username"
        + "[](bg:#a6d189 fg:#e78284)"
        + "$directory"
        + "[](fg:#a6d189 bg:#8caaee)"
        + "$git_branch"
        + "$git_status"
        + "[](fg:#8caaee bg:#ca9ee6)"
        + "$c"
        + "$elixir"
        + "$elm"
        + "$golang"
        + "$haskell"
        + "$java"
        + "$julia"
        + "$nodejs"
        + "$nim"
        + "$rust"
        + "[](fg:#ca9ee6 bg:#babbf1)"
        + "$time"
        + "[ ](fg:#babbf1)"
        + "$line_break"
        + "$character";

      command_timeout = 5000;
      add_newline = true;

      username = {
        show_always = true;
        style_user = "bg:#e78284 fg:#292c3c";
        style_root = "bg:#e78284";
        format = "[$user ]($style)";
      };

      directory = {
        style = "bg:#a6d189 fg:#292c3c";
        format = "[ $path ]($style)";
        truncation_length = 3;
        truncation_symbol = "../";

        substitutions = {
          "Documents" = " ";
          "Downloads" = " ";
          "Music" = " ";
          "Pictures" = " ";
        };
      };

      c = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      elixir = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      elm = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      git_branch = {
        symbol = "";
        style = "bg:#8caaee fg:#292c3c";
        format = "[ $symbol $branch ]($style)";
      };

      git_status = {
        style = "bg:#8caaee fg:#292c3c";
        format = "[$all_status$ahead_behind ]($style)";
      };

      golang = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      haskell = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      java = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      julia = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      nodejs = {
        symbol = "";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      nim = {
        symbol = " ";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      python = {
        style = "bg:#3B4252";
        format = "[(\($virtualenv\) )]($style)";
      };

      rust = {
        symbol = "";
        style = "bg:#ca9ee6 fg:#292c3c";
        format = "[ $symbol ($version) ]($style)";
      };

      time = {
        disabled = false;
        time_format = "%R"; # Hour:Minute Format
        style = "bg:#babbf1 fg:#292c3c";
        format = "[ $time ]($style)";
      };

      character = {
        format = "$symbol";
        success_symbol = " 󱞩 (fg:#00ff00)";
        error_symbol = " 󱞩 (fg:#ff0000)";
      };
    };
  };
}
