{ config, lib, pkgs, ... }: {
  boot.loader.systemd-boot.enable = (config.daedalus.bootloader == "systemd-boot");
}
