let
  colors = {
    # ayu-light
    base00 = "#f8f9fa";
    base01 = "#edeff1";
    base02 = "#d2d4d8";
    base03 = "#a0a6ac";
    base04 = "#8A9199";
    base05 = "#5c6166";
    base06 = "#4e5257";
    base07 = "#404447";
    base08 = "#f07171";
    base09 = "#fa8d3e";
    base0A = "#f2ae49";
    base0B = "#6cbf49";
    base0C = "#4cbf99";
    base0D = "#399ee6";
    base0E = "#a37acc";
    base0F = "#e6ba7e";
  };

  opacity = {
    terminal = 1.0;
    desktop = 1.0;
    applications = 1.0;
    popups = 1.0;
  };

  fonts = pkgs: let
    package = pkgs.nerd-fonts.iosevka;
    name = "Iosevka Nerd Font";
    serif = {
      inherit package;
      name = name + " Propo";
    };
  in {
    inherit serif;
    sansSerif = serif;
    monospace = {
      inherit package;
      name = name + " Mono";
    };
    emoji = {
      package = pkgs.noto-fonts-color-emoji;
      name = "Noto Color Emoji";
    };

    sizes = {
      terminal = 12;
      desktop = 10;
      applications = 12;
      popups = 10;
    };
  };

  image = ./wallpaper.jpg;

  stylix = pkgs: {
    enable = true;

    base16Scheme = colors;
    polarity = "dark";

    fonts = fonts pkgs;

    cursor = {
      package = pkgs.nordzy-cursor-theme;
      name = "Nordzy-cursors";
      size = 32;
    };

    inherit opacity image;
  };
in { inputs, pkgs, lib, ... }: {
  flake-file.inputs.stylix = {
    url = "github:danth/stylix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.modules.nixos.stylix = { pkgs, ... }: {
    imports = [
      inputs.stylix.nixosModules.stylix
    ];
    stylix = stylix pkgs;
  };

  flake.modules.homeManager.stylix = { pkgs, ... }: {
    imports = [
      inputs.stylix.homeModules.stylix
    ];
    stylix = stylix pkgs;
  };

  flake.lib.stylix = let
    createColorsFrom = pkgs: ((inputs.stylix.inputs.base16.lib { inherit pkgs lib; }).mkSchemeAttrs colors);
  in {
    inherit createColorsFrom;
    colors = createColorsFrom pkgs;
    inherit opacity image;
    fonts = fonts pkgs;
  };
}
