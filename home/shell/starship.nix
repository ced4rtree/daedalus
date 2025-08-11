{ config, lib, pkgs, ... }: let
  cfg = config.daedalus.home.shell;
in {
  programs.starship = lib.mkIf (cfg.zsh.enable || cfg.fish.enable) {
    enable = true;
    enableZshIntegration = config.daedalus.home.shell.zsh.enable;
    settings = let
      colors = config.lib.stylix.colors.withHashtag;
    in {
      format = 
        ''[](${colors.red})''
        + "$python"
        + "$username"
        + ''[](bg:${colors.green} fg:${colors.red})''
        + "$directory"
        + ''[](fg:${colors.green} bg:${colors.cyan})''
        + "$git_branch"
        + "$git_status"
        + ''[](fg:${colors.cyan} bg:${colors.blue})''
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
        + ''[](fg:${colors.blue} bg:${colors.magenta})''
        + "$time"
        + ''[ ](fg:${colors.magenta})''
        + "$line_break"
        + "$character";

      command_timeout = 5000;
      add_newline = true;

      username = {
        show_always = true;
        style_user = "bg:${colors.red} fg:${colors.base00}";
        style_root = "bg:${colors.red}";
        format = "[$user ]($style)";
      };

      directory = {
        style = "bg:${colors.green} fg:${colors.base00}";
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
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      elixir = {
        symbol = " ";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      elm = {
        symbol = " ";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      git_branch = {
        symbol = "";
        style = "bg:${colors.cyan} fg:${colors.base00}";
        format = "[ $symbol $branch ]($style)";
      };

      git_status = {
        style = "bg:${colors.cyan} fg:${colors.base00}";
        format = "[$all_status$ahead_behind ]($style)";
      };

      golang = {
        symbol = " ";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      haskell = {
        symbol = " ";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      java = {
        symbol = " ";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      julia = {
        symbol = " ";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      nodejs = {
        symbol = "";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      nim = {
        symbol = " ";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      python = {
        style = "bg:#3B4252";
        format = "[(\($virtualenv\) )]($style)";
      };

      rust = {
        symbol = "";
        style = "bg:${colors.blue} fg:${colors.base00}";
        format = "[ $symbol ($version) ]($style)";
      };

      time = {
        disabled = false;
        time_format = "%R"; # Hour:Minute Format
        style = "bg:${colors.magenta} fg:${colors.base00}";
        format = "[ $time ]($style)";
      };

      character = {
        format = "$symbol";
        success_symbol = " 󱞩 ";
        error_symbol = " 󱞩 ";
        vimcmd_symbol = " 󱞩 ";
        vimcmd_replace_one_symbol = " 󱞩 ";
        vimcmd_replace_symbol = " 󱞩 ";
        vimcmd_visual_symbol = " 󱞩 ";
      };
    };
  };
}
