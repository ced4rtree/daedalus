{ config, lib, pkgs, ... }: {
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
