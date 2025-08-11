{ config, lib, pkgs, ... }: {
  options.daedalus.host = {
    nvidia.enable = lib.mkEnableOption "nvidia";
  };
}
