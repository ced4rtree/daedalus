{ config, ... }: {
  flake.modules.homeManager.emacsBase = { lib, pkgs, nixpkgs, ... }: {
    home.packages = with pkgs; [
      mu
      python312Packages.mutagen
    ];

    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
        sha256 = "sha256:1s1661bd3ljlyvcwa89q21vr1njlkwx7x5zlryd044rprdf5crnq";
      }))
    ];

    services.emacs = {
      enable = true;
      defaultEditor = true;
    };

    programs.emacs.enable = true;

    # I have to add all the stylix stuff manually because the font behaves weird
    stylix.targets.emacs.enable = false;
  };
}
