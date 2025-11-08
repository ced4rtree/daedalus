{ config, ... }: {
  flake.modules.nixos.virt-manager = { pkgs, ... }: {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
    users.users.${config.daedalus.username}.extraGroups = [ "libvirtd" ];
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
