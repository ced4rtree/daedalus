{ config, ... }: {
  flake.modules.nixos.users = {
    users.users.${config.daedalus.username} = {
      isNormalUser = true;
      extraGroups = [ "networkmanager" "wheel" "input" "video" ];
      initialPassword = "pass";
    };
  };

  flake.modules.homeManager.users = {
    home.username = config.daedalus.username;
    home.homeDirectory = "/home/" + config.daedalus.username;
  };
}
