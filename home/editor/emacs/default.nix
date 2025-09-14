{ lib, config, pkgs, mylib, ... }: {
  imports = mylib.scanPaths ./.;

  options.daedalus.home.editor.emacs = {
    enable = lib.mkEnableOption "emacs";
    flavor = lib.mkOption {
      type = lib.types.enum [
        "vanilla"
        "doom"
      ];
      example = "doom";
    };
  };

  config = lib.mkIf config.daedalus.home.editor.emacs.enable {
    home.packages = with pkgs; [
      mu
      python312Packages.mutagen
    ];

    services.emacs = {
      enable = true;
      defaultEditor = true;
    };

    # I have to add all the stylix stuff manually because the font behaves weird
    stylix.targets.emacs.enable = false;

    programs.emacs.enable = true;
  };
}
