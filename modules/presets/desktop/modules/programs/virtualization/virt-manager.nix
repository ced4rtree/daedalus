{ config, ... }: {
  flake.modules.nixos.virtManager = { pkgs, ... }: {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
    users.users.${config.daedalus.username}.extraGroups = [ "libvirtd" ];

    daedalus.dconf."org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
}
