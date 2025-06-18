{ config, pkgs, lib, ... }: let
  user = "cedar";
  homeManagerSessionVars = "/etc/profiles/per-user/${user}/etc/profile.d/hm-session-vars.sh";
in {
  home.packages = with pkgs; [
    krabby
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = false;
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
      zstyle ':completion:*' menu select
      zmodload zsh/complist
      # Smarter completion initialization, only reload cache once a day
      autoload -Uz compinit
      if [ "$(date +'%j')" != "$(stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)" ]; then
          compinit
      else
          compinit -C
      fi
      _comp_options+=(globdots)

      # enable conda
      [ -f ~/.conda/etc/profile.d/conda.sh ] && command -v conda>/dev/null && source ~/.conda/etc/profile.d/conda.sh

      krabby random | awk 'NR>1 { print $0 }'
    '';

    sessionVariables = {
      PATH = "$PATH:$HOME/.local/bin/";
    };
  };
}
