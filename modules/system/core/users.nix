{ config, ... }: let
  username = config.daedalus.username;
in {
  flake.modules.nixos.users = { config, ... }: {
    sops.secrets.password.neededForUsers = true;
    users.users.${username} = {
      isNormalUser = true;
      extraGroups = [ "networkmanager" "wheel" "input" "video" ];
      hashedPasswordFile = config.sops.secrets.password.path;
    };
  };

  flake.modules.homeManager.users = {
    home = {
      inherit username;
      homeDirectory = "/home/${username}";
    };
  };
}
