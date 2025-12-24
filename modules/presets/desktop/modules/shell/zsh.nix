{ config, inputs, ... }: {
  flake.modules.nixos.zsh = { pkgs, ... }: let
    myPkgs = config.flake.packages.${pkgs.stdenv.hostPlatform.system};
  in {
    environment.shells = [ myPkgs.zsh ];
    users.users.${config.daedalus.username}.shell = myPkgs.zsh;
  };

  perSystem = { pkgs, lib, ... }: {
    packages.zsh = let
      zshDotDir = pkgs.stdenv.mkDerivation {
        name = "zsh-dot-dir";
        src = pkgs.writeText ".zshrc" ''
          # autosuggestions
          source ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh

          # history settings
          HISTSIZE="10000"
          SAVEHIST="10000"
          HISTFILE="/home/cedar/${config.daedalus.username}"
          mkdir -p "$(dirname "$HISTFILE")"

          # prompt config
          ${lib.getExe config.flake.packages.${pkgs.stdenv.hostPlatform.system}.microfetch}
          eval "$(${lib.getExe config.flake.packages.${pkgs.stdenv.hostPlatform.system}.starship} init zsh)"

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

          # direnv
          eval "$(${lib.getExe pkgs.direnv} hook zsh)"

          # syntax highlighting
          source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        '';

        dontUnpack = true;
        installPhase = ''
          mkdir -p $out
          cp $src $out/.zshrc
        '';
      };
    in inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.zsh;
      env = {
        ZDOTDIR = "${zshDotDir}";
      };
    };
  };
}
