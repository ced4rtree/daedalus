{ lib, config, ... }: {
  options.daedalus.shell = lib.mkOption {
    type = (config.flake.lib.createEnumFromDir {
      dir = ./.;
      excludedNames = [ "shell.nix" "starship.nix" ];
    });
    description = "The shell for the user to have";
    example = "zsh";
  };

  config.flake.modules.nixos.shell = {
    imports = [ config.flake.modules.nixos.${config.daedalus.shell} ];
  };
}
