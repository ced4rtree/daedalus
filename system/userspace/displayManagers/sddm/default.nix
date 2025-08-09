{ config, pkgs, ... }: let
  sddm-theme = pkgs.callPackage (import ./corners-theme.nix ./background.jpg) {};
in {
  imports = [
    ./sddm-avatar.nix
  ];

  environment.systemPackages = [
    sddm-theme
  ];

  services.displayManager.sddm = {
    enable = true;
    package = pkgs.libsForQt5.sddm;
    theme = "corners";
    wayland.enable = true;
  };
}
