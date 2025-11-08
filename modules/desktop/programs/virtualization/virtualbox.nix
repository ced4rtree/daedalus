{
  flake.modules.nixos.virtualbox = { config, ... }: {
    virtualisation.virtualbox.host.enable = true;
    users.extraGroups.vboxusers.members = [ config.daedalus.username ];
  };
}
