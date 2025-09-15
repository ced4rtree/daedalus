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

    base16Scheme = {
      # doom-one theme
      # https://github.com/doomemacs/themes/blob/master/themes/doom-one-theme.el
      base00 = "#282c34";
      base01 = "#1c1f24";
      base02 = "#202328";
      base03 = "#23272e";
      base04 = "#3f444a";
      base05 = "#BBC2CF";
      base06 = "#9ca0a4";
      base07 = "#DFDFDF";
      base08 = "#ff6c6b";
      base09 = "#da8548";
      base0A = "#ECBE7B";
      base0B = "#98be65";
      base0C = "#46D9FF";
      base0D = "#51AFEF";
      base0E = "#c678dd";
      base0F = "#de7e52"; # i made this one up
    };
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
