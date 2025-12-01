{ lib, ... }: {
  flake.modules.homeManager.terminalOption = {
    options.daedalus.terminalCommand = lib.mkOption {
      type = lib.types.str;
      description = "Command used to spawn the terminal";
      example = "footclient";
    };
  };
}
