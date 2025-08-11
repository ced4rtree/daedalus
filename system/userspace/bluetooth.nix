{ config, lib, pkgs, ... }: {
  options.daedalus.bluetooth.enable = lib.mkEnableOption "bluetooth";

  config.hardware.bluetooth = lib.mkIf config.daedalus.bluetooth.enable {
    enable = true;
    powerOnBoot = true;
  };
}
