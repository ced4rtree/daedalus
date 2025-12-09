{ config, lib, inputs, ... }: {
  flake.modules.homeManager.base = { pkgs, ... }: {
    imports = lib.attrVals (import ./_commonModules.nix) config.flake.modules.homeManager;

    home.stateVersion = "25.05";
    programs.home-manager.enable = true;
    home.packages = with pkgs; [
      # misc
      htop
      floorp-bin
      gh
      mpv
      freetube
      qbittorrent
      clonehero
      appimage-run
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
