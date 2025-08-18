{ config, pkgs, lib, ... }: {
  options.daedalus.wm.hyprland.enable = lib.mkEnableOption "hyprland";
  config.programs.hyprland.enable = config.daedalus.wm.hyprland.enable;
}
