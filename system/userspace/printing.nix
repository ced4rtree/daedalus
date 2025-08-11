{ config, lib, pkgs, ... }: {
  options.daedalus.printing.enable = lib.mkEnableOption "printing";
  config.services.printing.enable = config.daedalus.printing.enable;
}
