{ config, pkgs, lib, ... }: let
  user = "cedar";
  homeManagerSessionVars = "/etc/profiles/per-user/${user}/etc/profile.d/hm-session-vars.sh";
in {
  home.packages = with pkgs; [
    krabby
  ];

  programs.zsh = {
    enable = true;

    shellInit = ''
      # enable conda
      if test -e ~/.conda/etc/profile.d/conda.sh; and command -v conda;
        source ~/.conda/etc/profile.d/conda.sh
      end

      krabby random | awk 'NR>1 { print $0 }'
    '';

    sessionVariables = {
      PATH = "$PATH:$HOME/.local/bin/";
    };
  };
}
