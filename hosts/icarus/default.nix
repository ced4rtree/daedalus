{ config, lib, pkgs, mylib, ... }: {
  imports = mylib.scanPaths ./.;
  config.daedalus.host = {
    nvidia.enable = true;
    hostname = "icarus";
    isLaptop = true;
  };
}
