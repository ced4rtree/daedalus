{ config, ... }: {
  flake.modules.homeManager.desktop = { lib, ... }: {
    imports =
      lib.attrVals (import ./_commonModules.nix) config.flake.modules.homeManager
      ++ (with config.flake.modules.homeManager; [
        kitty
        hypridle
        files
        doomEmacs
        neovim
        noctalia
        fuzzel
      ]);
  };
}
