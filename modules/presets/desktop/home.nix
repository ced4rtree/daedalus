{ config, ... }: {
  flake.modules.homeManager.desktop = { lib, pkgs, ... }: {
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

    home.packages = with pkgs; [
      htop
      floorp-bin
      gh
      mpv
      qbittorrent
      clonehero
      steam-run
      gimp
      xfce.thunar
      isync
      pass
      drawio
      ispell
      steam
      wirelesstools
      btop
    ];
  };
}
