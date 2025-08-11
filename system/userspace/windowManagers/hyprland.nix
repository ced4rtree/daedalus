{ config, pkgs, lib, ... }: {
  options.daedalus.wm.hyprland.enable = lib.mkEnableOption "hyprland";
  config.programs.hyprland = lib.mkIf config.daedalus.wm.hyprland.enable {
    enable = true;
    withUWSM = true;
  };
}
