{ config, lib, pkgs, ... }: {
  home.file.".config/niri/config.kdl".source = ./config.kdl;
  home.packages = with pkgs; [
    xdg-desktop-portal-gtk
    xwayland-satellite
  ];
  services.swww.enable = true;
}
