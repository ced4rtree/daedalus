{ config, lib, pkgs, ... }: {
  programs.rofi = {
    enable = true;
    cycle = true;
    font = "Mono 12";
    modes = [ "drun" "emoji" ];
    theme = ./theme.rasi;
  }
}
