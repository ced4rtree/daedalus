{ config, lib, pkgs, ... }: {
  options.daedalus.home.launcher.rofi.enable = lib.mkEnableOption "rofi";

  config = lib.mkIf config.daedalus.home.launcher.rofi.enable {
    programs.rofi = {
      enable = true;
      cycle = true;
      font = "Mono 12";
      modes = [ "drun" "emoji" ];
      theme = ./theme.rasi;
      package = pkgs.rofi-wayland;
    };
  };
}
