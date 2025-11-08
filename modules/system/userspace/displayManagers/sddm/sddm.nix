{
  flake.modules.nixos.sddm = { config, lib, pkgs, ... }: let
    sddm-theme = pkgs.callPackage (import ./_corners-theme.nix {
      background = ./background.jpg;
      colors = config.lib.stylix.colors.withHashtag;
    }) {};
  in {
    environment.systemPackages = [
      sddm-theme
      pkgs.kdePackages.qt5compat
    ];

    services.displayManager.sddm = {
      enable = true;
      package = pkgs.kdePackages.sddm;
      theme = "corners";
      wayland.enable = true;
    };

    # the below code found here: https://www.reddit.com/r/NixOS/comments/1cot084/is_there_way_to_make_sddm_to_display_users_avatars/
    # Define systemd service to run on boot
    systemd.services."sddm-avatar" = {
      description = "Service to copy or update users Avatars at startup.";
      wantedBy = [ "multi-user.target" ];
      before = [ "sddm.service" ];
      script = ''
      for user in /home/*; do

        username=$(basename "$user")
        icon_source="$user/.face.icon"
        icon_dest="/var/lib/AccountsService/icons/$username"

        if [ -f "$icon_source" ]; then
          if [ ! -f "$icon_dest" ] || ! cmp -s "$icon_source" "$icon_dest"; then
            rm -f "$icon_dest"
            mkdir -p $(dirname $icon_dest)
            cp -L "$icon_source" "$icon_dest"
          fi
        fi

      done
    '';
      serviceConfig = {
        Type = "simple";
        User = "root";
        StandardOutput = "journal+console";
        StandardError = "journal+console";
      };
    };

    # Ensures SDDM starts after the service.
    systemd.services.sddm = {
      after = [ "sddm-avatar.service" ];
    };
  };
}
