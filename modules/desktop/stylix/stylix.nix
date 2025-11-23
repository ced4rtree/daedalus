let
  stylix = { config, pkgs }: {
    enable = true;

    base16Scheme = {
      base00 = "#000000";
      base01 = "#1c1f24";
      base02 = "#21242b";
      base03 = "#3f444a";
      base04 = "#5B6268";
      base05 = "#BBC2CF";
      base06 = "#9ca0a4";
      base07 = "#DFDFDF";
      base08 = "#f966a1";
      base09 = "#f9ab66";
      base0A = "#edd078";
      base0B = "#b5c77d";
      base0C = "#6dd3c0";
      base0D = "#7fc6f7";
      base0E = "#d194fc";
      base0F = "#de7e52"; # i made this one up
    };
    polarity = "dark";

    cursor = {
      package = pkgs.nordzy-cursor-theme;
      name = "Nordzy-cursors";
      size = 32;
    };

    fonts = let
      package = pkgs.nerd-fonts.monaspace;
      name = "MonaspiceNe Nerd Font";
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
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };

      sizes.terminal = 12;
    };

    opacity = {
      terminal = 0.7;
      desktop = 0.7;
    };

    image = ./wallpaper.jpg;
  };
in {
  flake.modules.nixos.stylix = { config, pkgs, inputs, ... }: {
    imports = [
      inputs.stylix.nixosModules.stylix
    ];
    config = {
      stylix = stylix { inherit config pkgs; };
    };
  };

  flake.modules.homeManager.stylix = { config, pkgs, inputs, ... }: {
    imports = [
      inputs.stylix.homeModules.stylix
    ];
    config = {
      stylix = stylix { inherit config pkgs; };
    };
  };
}
