{ config, ... }: {
  flake.modules.nixos.nix = {
    nix.settings = {
      substituters = [
        "https://nix-community.cachix.org"
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];

      experimental-features = [
        "nix-command"
        "flakes"
        "pipe-operators"
      ];

      warn-dirty = false;
    };

    programs.nh = {
      enable = true;
      flake = "/home/" + config.daedalus.username + "/.dotfiles";
    };
  };
}
