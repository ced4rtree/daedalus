{ config, ... }: let
  topConfig = config;
in {
  flake.modules.nixos.zsh = { pkgs, ... }: {
    programs.zsh.enable = true;
    programs.zsh.syntaxHighlighting.enable = true;
    environment.shells = [ pkgs.zsh ];
    users.users.${topConfig.daedalus.username}.shell = pkgs.zsh;
  };

  flake.modules.homeManager.zsh = { config, pkgs, lib, ... }: {
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

      initContent = /* bash */ ''
        # call my patched version of microfetch
        ${lib.getExe topConfig.flake.packages.${pkgs.stdenv.hostPlatform.system}.microfetch}

        function git_branch_name() {
          branch=$(git symbolic-ref HEAD 2>/dev/null | tr '/' '\n' | tail -n 1)
          if [[ $branch == "" ]]; then
            echo ""
          else
            echo "$branch "
          fi
        }

        setopt prompt_subst

        blue=$(tput setaf 4)
        green=$(tput setaf 2)
        magenta=$(tput setaf 5)
        reset=$(tput sgr0)
        prompt='$blue%~ $green$(git_branch_name)$magenta->$reset '

        zstyle ':completion:*' menu select
        zmodload zsh/complist
        # Smarter completion initialization, only reload cache once a day
        autoload -Uz compinit
        if [ "$(date +'%j')" != "$(stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)" ]; then
          compinit
        else
          compinit C
        fi
        _comp_options+=(globdots)
      '';
    };
  };
}
