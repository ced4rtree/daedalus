{ config, pkgs, ... }: let
  sddm-gruvbox = pkgs.callPackage ./gruvbox-theme.nix {};
in {
  environment.systemPackages = [
    sddm-gruvbox
  ];

  services.displayManager.sddm = {
    enable = true;
    package = pkgs.kdePackages.sddm;
    theme = "sddm-gruvbox";
    wayland.enable = true;
  };
}
