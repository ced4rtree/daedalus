{
  flake.modules.nixos.virt-manager = { config, pkgs, ... }: {
    virtualisation.libvirtd.qemu.ovmf.packages = [
      pkgs.pkgsCross.aarch64-multiplatform.OVMF.fd # AAVMF
      pkgs.OVMF.fd
    ];

    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
    users.users.${config.settings.username}.extraGroups = [ "libvirtd" ];
  };

  flake.modules.homeManager.virt-manager = {
    dconf.settings = {
      "org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    };
  };
}
