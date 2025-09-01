{ config, lib, pkgs, ... }: let
  sddm-theme = pkgs.callPackage (import ./corners-theme.nix {
    background = ./background.jpg;
    colors = config.lib.stylix.colors.withHashtag;
  }) {};
in {
  imports = [
    ./sddm-avatar.nix
  ];

  config = lib.mkIf (config.daedalus.displayManager == "sddm") {
    environment.systemPackages = [
      sddm-theme
      pkgs.kdePackages.qt5compat
    ];

    services.displayManager.sddm = {
      enable = true;
      package = pkgs.kdePackages.sddm;
      theme = "corners";
      wayland.enable = true;
    };
  };
}
