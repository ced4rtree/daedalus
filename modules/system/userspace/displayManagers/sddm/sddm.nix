{
  flake.modules.nixos.sddm = { config, lib, pkgs, inputs, ... }: let
    #sddm-theme = pkgs.callPackage (import ./_corners-theme.nix {
    #background = ./background.jpg;
    #colors = config.lib.stylix.colors.withHashtag;
    #}) {};
    background = {
      outPath = ./background.jpg;
      name = "background";
    };
    sddm-theme = inputs.silent-sddm.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
      theme = "default";
      extraBackgrounds = [background];
      theme-overrides = {
        "LoginScreen.LoginArea.Avatar" = {
          shape = "circle";
          active-border-color = "#ffcfce";
        };
        "LoginScreen" = {
          background = "${background.name}";
          blur = 32;
        };
        "LockScreen" = {
          background = "${background.name}";
          blur = 0;
        };       
      };
    };
  in {
   # include the test package which can be run using test-sddm-silent
   environment.systemPackages = [sddm-theme sddm-theme.test];
   qt.enable = true;
   services.displayManager.sddm = {
      wayland.enable = true;
      package = pkgs.kdePackages.sddm; # use qt6 version of sddm
      enable = true;
      theme = sddm-theme.pname;
      # the following changes will require sddm to be restarted to take
      # effect correctly. It is recomend to reboot after this
      extraPackages = sddm-theme.propagatedBuildInputs;
      settings = {
        # required for styling the virtual keyboard
        General = {
          GreeterEnvironment = "QML2_IMPORT_PATH=${sddm-theme}/share/sddm/themes/${sddm-theme.pname}/components/,QT_IM_MODULE=qtvirtualkeyboard";
          InputMethod = "qtvirtualkeyboard";
        };
      };
   };
  };




  #environment.systemPackages = [
  #sddm-theme
  #pkgs.kdePackages.qt5compat
  #];
  #
  #services.displayManager.sddm = {
  #enable = true;
  #package = pkgs.kdePackages.sddm;
  #theme = "corners";
  #wayland.enable = true;
  #};
  #
  ## the below code found here: https://www.reddit.com/r/NixOS/comments/1cot084/is_there_way_to_make_sddm_to_display_users_avatars/
  ## Define systemd service to run on boot
  #systemd.services."sddm-avatar" = {
  #description = "Service to copy or update users Avatars at startup.";
  #wantedBy = [ "multi-user.target" ];
  #before = [ "sddm.service" ];
  #script = ''
  #for user in /home/*; do
  #
  #username=$(basename "$user")
  #icon_source="$user/.face.icon"
  #icon_dest="/var/lib/AccountsService/icons/$username"
  #
  #if [ -f "$icon_source" ]; then
  #if [ ! -f "$icon_dest" ] || ! cmp -s "$icon_source" "$icon_dest"; then
  #rm -f "$icon_dest"
  #mkdir -p $(dirname $icon_dest)
  #cp -L "$icon_source" "$icon_dest"
  #fi
  #fi
  #
  #done
  #'';
  #serviceConfig = {
  #Type = "simple";
  #User = "root";
  #StandardOutput = "journal+console";
  #StandardError = "journal+console";
  #};
  #};
  #
  ## Ensures SDDM starts after the service.
  #systemd.services.sddm = {
  #after = [ "sddm-avatar.service" ];
  #};
  #};
}
