{ config, lib, ... }: {
  options.daedalus.terminal.program = lib.mkOption {
    # take all files/directories in ./. and use them as allowed options
    type = config.flake.lib.createEnumFromDir {
      dir = ./.;
      excludedNames = [ "terminal.nix" ];
    };
    description = "Terminal to configure as default";
    example = "foot";
    readOnly = true;
  };

  options.daedalus.terminal.command = lib.mkOption {
    type = lib.types.str;
    description = "Command to spawn the terminal";
    example = "footclient";
  };

  config.flake.modules.nixos.terminal = {
    imports = [ config.flake.modules.nixos.${config.daedalus.terminal.program} ];
  };
}
