{ lib, inputs, pkgs, config, ... }: {
  system.userActivationScripts = {
    splash = {
      text = "${lib.getExe' inputs.dark-text.packages.${pkgs.system}.default "dark-text"}";
      deps = [];
    };
  };
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
}
