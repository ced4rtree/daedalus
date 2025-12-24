{ config, lib, ... }: {
  options.daedalus.lockscreen.program = lib.mkOption {
    type = let
      lockPrograms = config.flake.lib.filenamesIn {
        dir = ./.;
        excludedNames = [ "lock.nix" ];
      };
    in lib.types.enum (lockPrograms ++ [ "noctalia-shell" ]);
    description = "What program to use for locking the screen";
    example = "hyprlock";
  };

  options.daedalus.lockscreen.command = lib.mkOption {
    type = lib.types.str;
    description = "Shell command that will invoke the desktop lock screen";
    example = "noctalia-shell ipc call lockScreen lock";
  };
}
