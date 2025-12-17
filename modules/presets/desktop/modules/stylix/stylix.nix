let
  colors = {
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

  opacity = {
    terminal = 0.7;
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

    sizes.terminal = 12;
  };

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

    inherit opacity;

    image = ./wallpaper.jpg;
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

  flake.lib.stylix = {
    colors = ((inputs.stylix.inputs.base16.lib { inherit pkgs lib; }).mkSchemeAttrs colors);
    inherit opacity;
    fonts = fonts pkgs;
  };
}
