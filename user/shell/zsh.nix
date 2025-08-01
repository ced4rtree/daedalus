{ config, pkgs, lib, ... }: {
  home.packages = with pkgs; [
    krabby
  ];

  imports = [
    ./starship.nix
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
      # vim mode
      bindkey -v
      export KEYTIMEOUT=1

      # restore the emacs-like C-p and C-n for history
      bindkey -v '^P' up-history
      bindkey -v '^N' down-history

      # Change cursor shape for different vi modes.
      function zle-keymap-select () {
          case $KEYMAP in
              vicmd) echo -ne '\e[1 q';;      # block
              viins|main) echo -ne '\e[5 q';; # beam
          esac
      }
      zle -N zle-keymap-select
      zle-line-init() {
          zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
          echo -ne "\e[5 q"
      }
      zle -N zle-line-init
      echo -ne '\e[5 q' # Use beam shape cursor on startup.
      preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

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
  };
}
