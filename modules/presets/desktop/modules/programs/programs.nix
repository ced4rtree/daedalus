{ config, ... }: let
in {
  flake.modules.nixos.desktopPrograms = { lib, pkgs, ... }: {
    imports = with config.flake.modules.nixos; [
      # misc
      batsignal
      homestuck
      xdg
      virtManager
      dconf

      # music
      mpd

      # internet
      # firefox
      qutebrowser
      discord

      # cli
      gpg
      direnv
    ];
  };
}
