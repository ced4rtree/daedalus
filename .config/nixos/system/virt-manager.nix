{ config, pkgs, ... }: {
  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;
  users.users.cedar.extraGroups = [ "libvirtd" ];
}
