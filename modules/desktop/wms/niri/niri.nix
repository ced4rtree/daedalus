{
  flake.modules.nixos.niri = {
    programs.niri.enable = true;
  };

  flake.modules.homeManager.niri = { config, lib, pkgs, ... }: {
    xdg.configFile."niri/config.kdl".text = lib.replaceStrings
      [ "@blue" "@magenta" "@red" "@gray" ]
      (with config.lib.stylix.colors; [
        withHashtag.base0D
        withHashtag.base0E
        withHashtag.base08
        withHashtag.base04
      ])
      (builtins.readFile ./config.kdl);
    xdg.configFile."niri/niri-portals.conf".source = ./niri-portals.conf;
    home.packages = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
      gnome-keyring
      xwayland-satellite
    ];
  };
}
