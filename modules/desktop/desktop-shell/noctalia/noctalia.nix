{ config, ... }: let
  username = config.daedalus.username;
in {
  flake.modules.homeManager.noctalia = { lib, pkgs, inputs, config, ...}: {
    home.packages = with inputs; [
      quickshell.packages.${pkgs.system}.default
      noctalia-shell.packages.${pkgs.system}.default
    ] ++ (with pkgs; [
      # deps
      wlsunset
      brightnessctl
      ddcutil
      cliphist
      cava
    ]);

    sops.secrets.noctalia_location = { };

    xdg.configFile."noctalia/colors.json" = {
      text = builtins.replaceStrings
        [
          "ERROR"
          "ON_ERROR"
          "ON_PRIMARY"
          "ON_SECONDARY"
          "ON_SURFACE"
          "ON_SURFACE_VARIANT"
          "ON_TERTIARY"
          "OUTLINE"
          "PRIMARY"
          "SECONDARY"
          "SHADOW"
          "SURFACE"
          "SURFACE_VARIANT"
          "TERTIARY"
        ]
        (with config.lib.stylix.colors.withHashtag; [
          base08 # error
          base03 # onError
          base03 # onPrimary
          base03 # onSecondary
          base07 # onSurface
          base05 # onSurfaceVariant
          base03 # onTertiary
          base04 # outline
          base0D # primary
          base0E # secondary
          base03 # shadow
          base00 # surface
          base06 # surfaceVariant
          base0E # tertiary
        ])
        (builtins.readFile ./colors.json);
      force = true;
    };
    xdg.configFile."noctalia/settings.json" = {
      text = let
        videoDirectory = "/home/${username}/Videos";
        font = config.stylix.fonts.sansSerif.name;
        wallpaper = config.stylix.image;
        wallpaperDir = builtins.dirOf wallpaper;
        avatarImage = "/home/${username}/.face.icon";
      in builtins.replaceStrings
        [ "VIDEO_DIRECTORY" "FONT" "WALLPAPER_DIRECTORY" "WALLPAPER_FILE" "AVATAR_IMAGE" ]
        [ videoDirectory font wallpaperDir wallpaper avatarImage ]
        (builtins.readFile ./settings.json);
      force = true;
    };

    # Hack to replace the string LOCATION with my actual location in
    # ~/.config/noctalia/settings.json while maintaining purity.
    # Reading from the secrets file during eval is impure.
    # This does make the file a normal file instead of a symlink to the store, but oh well.
    home.activation = {
      addNoctaliaLocation = config.lib.dag.entryAfter ["reloadSystemd"] ''
        run sed -i "s/LOCATION/$(cat ${config.sops.secrets.noctalia_location.path})/" /home/${username}/.config/noctalia/settings.json
      '';
    };

    systemd.user.services.noctalia-shell = {
      Unit = {
        Description = "Run Noctalia Shell";
        After = "graphical-session.target";
        BindsTo = "graphical-session.target";
        PartOf = "graphical-session.target";
        Requisite = "graphical-session.target";
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.writeShellScript "noctalia-shell" ''
          #!${pkgs.bash}/bin/bash
          ${lib.getExe inputs.noctalia-shell.packages.${pkgs.system}.default}
        ''}";
      };
    };
  };
}
