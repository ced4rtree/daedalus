{
  flake-file.inputs.silent-sddm.url = "github:uiriansan/SilentSDDM";

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
          active-border-color = "${config.lib.stylix.colors.withHashtag.blue}";
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
    environment.systemPackages = [
      sddm-theme
      sddm-theme.test
      pkgs.kdePackages.qt5compat
    ];
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
}
