{ config, ... }: let
  inherit (config.daedalus) username;
in {
  flake.modules.nixos.users = { config, ... }: {
    sops.secrets.password.neededForUsers = true;
    users.users.${username} = {
      isNormalUser = true;
      extraGroups = [ "networkmanager" "wheel" "input" "video" ];
      hashedPasswordFile = config.sops.secrets.password.path;
    };
  };
}
