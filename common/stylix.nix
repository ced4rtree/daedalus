isHomeManager: { config, lib, pkgs, ... }: let
  icons = {
    enable = true;
    package = pkgs.candy-icons;
    dark = "candy-icons";
    light = "candy-icons";
  };
in {
  options.daedalus.stylix.enable = lib.mkEnableOption "stylix";
  options.daedalus.home.stylix.enable = lib.mkEnableOption "stylix";

  config.stylix = lib.mkIf
    ((config.daedalus.stylix.enable && !isHomeManager)
     || (config.daedalus.home.stylix.enable && isHomeManager)) {
    enable = true;

    base16Scheme = "${pkgs.base16-schemes}/share/themes/synth-midnight-dark.yaml";
    polarity = "dark";

    inherit (if isHomeManager then icons else {});

    cursor = {
      package = pkgs.nordzy-cursor-theme;
      name = "Nordzy-cursors";
      size = 32;
    };

    fonts = let
      package = pkgs.nerd-fonts.iosevka;
      name = "Iosevka Nerd Font";
    in {
      serif = {
        inherit package;
        name = name + " Propo";
      };
      sansSerif = config.stylix.fonts.serif;
      monospace = {
        inherit package;
        name = name + " Mono";
      };
      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };

      sizes.terminal = 15;
    };
  };
}
