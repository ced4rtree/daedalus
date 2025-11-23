{ config, ... }: let
  sharedModules = [ "virt-manager" ];
in {
  flake.modules.homeManager.desktopPrograms = { lib, ... }: {
    imports = with config.flake.modules.homeManager; [
      # misc
      batsignal
      homestuck
      xdg

      # music
      mpd
      mpris

      # internet
      # firefox
      qutebrowser
      discord

      # cli
      gpg
      direnv
      fastfetch
    ] ++ lib.attrVals sharedModules config.flake.modules.homeManager;
  };

  flake.modules.nixos.desktopPrograms = { lib, ... }: {
    imports = with config.flake.modules.nixos; [
      nh
    ] ++ lib.attrVals sharedModules config.flake.modules.nixos;
  };
}
