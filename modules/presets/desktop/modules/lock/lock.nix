{ config, lib, ... }: let
  lockPrograms = config.flake.lib.filenamesIn {
    dir = ./.;
    excludedNames = [ "lock.nix" ];
  };
in {
  options.daedalus.lockscreen.program = lib.mkOption {
    type = lib.types.enum (lockPrograms ++ [ "noctalia-shell" ]);
    description = "What program to use for locking the screen";
    example = "hyprlock";
  };

  options.daedalus.lockscreen.commandFor = let
    inherit (config.flake.lib) systemSpecific;
    inherit (lib.types) str;
  in lib.mkOption {
    type = systemSpecific str;
    description = "Shell command that will invoke the desktop lock screen";
    example = "noctalia-shell ipc call lockScreen lock";
  };

  config.flake.modules.nixos.lockscreen = {
    imports = let
      lockscreen = config.daedalus.lockscreen.program;

      # program not being a "lockscreen" meaning something like a desktop shell
      # such as noctalia instead of a standalone program. i.e. not in this
      # directory
      programIsLockscreen = lib.elem lockscreen lockPrograms;
    in lib.optional programIsLockscreen config.flake.modules.nixos.${lockscreen};
  };
}
