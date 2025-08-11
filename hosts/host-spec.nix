{ config, lib, pkgs, ... }: {
  options.daedalus.host = {
    nvidia.enable = lib.mkEnableOption "nvidia";
    hostname = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      example = "icarus";
    };
    isLaptop = lib.mkEnableOption "isLaptop";
  };
}
