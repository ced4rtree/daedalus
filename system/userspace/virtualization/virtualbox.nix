{ config, lib, pkgs, ... }: {
  options.daedalus.virtualization.virtualbox.enable = lib.mkEnableOption "virtualbox";

  config = lib.mkIf config.daedalus.virtualization.virtualbox.enable {
    virtualisation.virtualbox.host.enable = true;
    users.extraGroups.vboxusers.members = [ "cedar" ];
  };
}
