let
  stylix = { config, pkgs }: {
    enable = true;

    base16Scheme = {
      # doom-one theme
      # https://github.com/doomemacs/themes/blob/master/themes/doom-one-theme.el
      base00 = "#282c34";
      base01 = "#1c1f24";
      base02 = "#202328";
      base03 = "#3f444a";
      base04 = "#5B6268";
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
