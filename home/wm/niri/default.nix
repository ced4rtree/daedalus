{ config, lib, pkgs, ... }: {
  options.daedalus.home.wm.niri.enable = lib.mkEnableOption "niri";

  config = lib.mkIf config.daedalus.home.wm.niri.enable {
    home.file.".config/niri/config.kdl".source = ./config.kdl;
    home.packages = with pkgs; [
      xdg-desktop-portal-gtk
      xwayland-satellite
    ];
  };
}
