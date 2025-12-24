{ config, lib, ... }: {
  options.daedalus.terminal = lib.mkOption {
    # take all files/directories in ./. and use them as allowed options
    type = config.flake.lib.createEnumFromDir {
      dir = ./.;
      excludedNames = [ "option.nix" ];
    };
    description = "Terminal to configure as default";
    example = "foot";
    readOnly = true;
  };

  options.daedalus.terminalCommand = lib.mkOption {
    type = lib.types.str;
    description = "Command to spawn the terminal";
    default = config.daedalus.terminal;
  };

  config.flake.modules.nixos.terminal = {
    imports = [ config.flake.modules.nixos.${config.daedalus.terminal} ];
  };
}
