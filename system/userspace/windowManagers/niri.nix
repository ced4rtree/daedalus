{ config, pkgs, lib, ... }: {
  options.daedalus.wm.niri.enable = lib.mkEnableOption "niri";
  config.programs.niri.enable = config.daedalus.wm.niri.enable;
}
