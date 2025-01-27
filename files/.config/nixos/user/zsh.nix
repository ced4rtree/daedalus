{ config, pkgs, lib, ... }: let
  user = "cedar";
  homeManagerSessionVars = "/etc/profiles/per-user/${user}/etc/profile.d/hm-session-vars.sh";
in {
  home.packages = with pkgs; [
    zsh
    zsh-syntax-highlighting
    starship
    pokemon-colorscripts-mac
  ];

  programs.zsh = {
    enable = false;
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

    initExtra = ''
      autoload -U compinit
      zstyle ':completion:*' menu select
      zmodload zsh/complist
      compinit
      _comp_options+=(globdots)

      lfcd () {
        tmp="$(mktemp)"
        lf -last-dir-path="$tmp" "$@"
        if [ -f "$tmp" ]; then
          dir="$(cat "$tmp")"
          rm -f "$tmp"
          [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
        fi
      }

      pokemon-colorscripts -r | awk 'NR>1 { print $0 }'
      eval "$(starship init zsh)"
    '';

    sessionVariables = {
      PATH = "$PATH:$HOME/.local/bin/";
    };
  };
}
