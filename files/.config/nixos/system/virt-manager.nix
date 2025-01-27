{ config, pkgs, ... }: {
  virtualisation.libvirtd.qemu.ovmf.packages = [
    pkgs.pkgsCross.aarch64-multiplatform.OVMF.fd # AAVMF
    pkgs.OVMF.fd
  ];

  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;
  users.users.cedar.extraGroups = [ "libvirtd" ];
}
