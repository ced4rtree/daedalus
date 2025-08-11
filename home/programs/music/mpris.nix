{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.music.mpris.enable = lib.mkEnableOption "mpris";
  config.services.mpris-proxy.enable = config.daedalus.home.programs.music.mpris.enable;
}
