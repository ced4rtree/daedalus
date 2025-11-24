{ config, ... }: let
  topConfig = config;
in {
  flake.modules.nixos.zsh = { pkgs, ... }: {
    programs.zsh.enable = true;
    programs.zsh.syntaxHighlighting.enable = true;
    environment.shells = [ pkgs.zsh ];
    users.users.${topConfig.daedalus.username}.shell = pkgs.zsh;
  };

  flake.modules.homeManager.zsh = { config, pkgs, ... }: {
    imports = [
      topConfig.flake.modules.homeManager.starship
    ];

    programs.zsh = {
      enable = true;
      enableCompletion = false;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      autocd = true;

      dotDir = config.xdg.configHome + "/zsh";

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

        fastfetch
      '';
    };
  };
}
