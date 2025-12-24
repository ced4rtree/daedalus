{ lib, config, ... }: {
  options.daedalus.windowManager = lib.mkOption {
    type = (config.flake.lib.createEnumFromDir {
      dir = ./.;
      excludedNames = [ "windowManager.nix" ];
    });
    description = "What window manager to choose";
    example = "niri";
  };

  config.flake.modules.nixos.windowManager = {
    imports = [ config.flake.modules.nixos.${config.daedalus.windowManager} ];
  };
}
