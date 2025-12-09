{ config, ... }: {
  flake.modules.homeManager.emacsBase = { lib, pkgs, ... }: {
    home.packages = with pkgs; [
      mu
      python312Packages.mutagen
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
