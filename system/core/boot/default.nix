{ config, lib, pkgs, mylib, ... }: {
  imports = mylib.scanPaths ./.;
  options.daedalus.bootloader = lib.mkOption {
    type = lib.types.enum [
      "grub"
      "systemd-boot"
    ];
    default = "grub";
    example = "systemd-boot";
  };
}
