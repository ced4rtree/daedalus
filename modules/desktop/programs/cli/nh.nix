{ config, ... }: {
  flake.modules.nixos.nh = {
    programs.nh = {
      enable = true;
      flake = "/home/" + config.daedalus.username + "/.dotfiles";
    };
  };
}
