{ config, ... }: {
  flake.modules.homeManager.desktopPrograms = {
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

      # virtualization
      virt-manager
    ];
  };
}
