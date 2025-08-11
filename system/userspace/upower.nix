{ config, pkgs, lib, ... }: {
  options.daedalus.upower.enable = lib.mkEnableOption "upower";

  config.services = lib.mkIf config.daedalus.upower.enable {
    upower.enable = true;
    power-profiles-daemon.enable = true;
  };
}
