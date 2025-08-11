{ config, lib, pkgs, ... }: {
  options.daedalus.security.sudo.enable = lib.mkEnableOption "sudo";
  config.security.sudo.enable = config.daedalus.security.sudo.enable;
}
