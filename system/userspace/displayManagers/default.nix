{ config, lib, pkgs, mylib, ... }: {
  imports = mylib.scanPaths ./.;
  options.daedalus.displayManager = lib.mkOption {
    type = lib.types.enum [
      "ly"
      "sddm"
    ];
    default = "sddm";
    example = "ly";
  };
}
