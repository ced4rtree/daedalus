{ inputs, config, ... }: {
  flake.modules.nixos.niri = { lib, pkgs, ... }: {
    programs.niri = {
      enable = true;
      package = config.flake.packages.${pkgs.stdenv.hostPlatform.system}.niri;
    };

    xdg.configFile."niri/config.kdl".text = 
      (builtins.readFile ./config.kdl);

    xdg.portal.config.niri = {
      "default" = "gnome;gtk";
      "org.freedesktop.impl.portal.Access" = "gtk";
      "org.freedesktop.impl.portal.Notification" = "gtk";
      "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
      "org.freedesktop.impl.portal.FileChooser" = "gtk";
    };

    hj.packages = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
      gnome-keyring
      xwayland-satellite
    ];
  };

  perSystem = { pkgs, lib, ... }: let
    niriConfig = pkgs.writeText
      "niri-config"
      (lib.replaceStrings
        [ "@blue" "@magenta" "@red" "@gray" "@terminal" ]
        (with config.flake.lib.stylix.colors.withHashtag; [
          base0D
          base0E
          base08
          base04
          config.daedalus.terminal.command
        ])
        (builtins.readFile ./config.kdl));
  in {
    packages.niri = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.niri;
      flags = {
        "-c" = "${niriConfig}";
      };
    };
  };
}
