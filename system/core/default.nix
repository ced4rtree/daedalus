{ config, lib, pkgs, ... }: {
  imports = [
    ./graphics.nix
    ./grub.nix
    ./hardware-configuration.nix
    ./plymouth.nix
  ];
}
