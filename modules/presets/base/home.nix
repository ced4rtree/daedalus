{ config, lib, inputs, ... }: {
  flake-file.inputs.home-manager.url = "github:nix-community/home-manager";

  flake.modules.homeManager.base = { pkgs, ... }: {
    imports = lib.attrVals (import ./_commonModules.nix) config.flake.modules.homeManager;

    home.stateVersion = "25.05";
    programs.home-manager.enable = true;
    home.packages = with pkgs; [
      appimage-run
    ];
  };
}
