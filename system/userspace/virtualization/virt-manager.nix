{ config, lib, pkgs, ... }: {
  options.daedalus.virtualization.virt-manager.enable = lib.mkEnableOption "virt-manager";

  config = lib.mkIf config.daedalus.virtualization.virt-manager.enable {
    virtualisation.libvirtd.qemu.ovmf.packages = [
      pkgs.pkgsCross.aarch64-multiplatform.OVMF.fd # AAVMF
      pkgs.OVMF.fd
    ];

    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
    users.users.cedar.extraGroups = [ "libvirtd" ];
  };
}
