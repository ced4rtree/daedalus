{ config, lib, pkgs, ... }: {
  options.daedalus.home.wallpaper.hyprpaper.enable = lib.mkEnableOption "hyprpaper";
  config.services.hyprpaper.enable = config.daedalus.home.wallpaper.hyprpaper.enable;
}
