{
  flake.modules.nixos.niri = {
    programs.niri.enable = true;
  };

  flake.modules.homeManager.niri = { config, lib, pkgs, ... }: {
      xdg.configFile."niri/config.kdl".text = lib.replaceStrings
        [ "@blue" "@magenta" "@red" "@gray" ]
        (with config.lib.stylix.colors; [
          withHashtag.blue
          withHashtag.magenta
          withHashtag.red
          base03
        ])
        (builtins.readFile ./config.kdl);
      xdg.configFile."niri/niri-portals.conf".source = ./niri-portals.conf;
      home.packages = with pkgs; [
        xdg-desktop-portal-gtk
        xwayland-satellite
      ];
  };
}
