isHomeManager: { config, lib, pkgs, ... }: let
  icons = {
    enable = true;
    package = pkgs.candy-icons;
    dark = "candy-icons";
    light = "candy-icons";
  };
in {
  stylix = {
    enable = true;

    image = ./gruvbox-dark-rainbow.png;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark.yaml";
    polarity = "dark";

    inherit (if isHomeManager then icons else {});

    cursor = {
      package = pkgs.nordzy-cursor-theme;
      name = "Nordzy-cursors";
      size = 32;
    };

    opacity = {
      terminal = 0.65; 
      applications = 0.65;
    };

    fonts = {
      serif = config.stylix.fonts.monospace;
      sansSerif = config.stylix.fonts.monospace;
      monospace = {
        package = pkgs.nerd-fonts.jetbrains-mono;
        name = "JetBrainsMono Nerd Font";
      };

      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };
  };
}
