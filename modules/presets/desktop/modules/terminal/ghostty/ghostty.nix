{ inputs, config, ... }: {
  flake.modules.nixos.ghostty = { pkgs, lib, ... }: let
    inherit (config.flake.lib) stylix;
  in {
    hj.packages = [ pkgs.ghostty ];

    hj.xdg.config.files = {
      "ghostty/config" = {
        generator = pkgs.formats.keyValue {
          listsAsDuplicateKeys = true;
          mkKeyValue = lib.generators.mkKeyValueDefault { } " = ";
        };

        value = {
          gtk-single-instance = true;
          custom-shader = [ "${./cursor_blaze.glsl}" ];

          font-family = with stylix.fonts; [
            monospace.name
            emoji.name
          ];

          font-size = stylix.fonts.sizes.terminal;

          background-opacity = stylix.opacity.terminal;

          theme = "stylix";
        };
      };

      "ghostty/themes/ghostty-stylix-theme".text = with stylix.colors.withHashtag; ''
        background = ${base00};
        foreground = ${base05};
        cursor-color = ${base05};
        selection-background = ${base02};
        selection-foreground = ${base05};
  
        palette = 0=${base00}
        palette = 1=${base08}
        palette = 2=${base0B}
        palette = 3=${base0A}
        palette = 4=${base0D}
        palette = 5=${base0E}
        palette = 6=${base0C}
        palette = 7=${base05}
        palette = 8=${base03}
        palette = 9=${base08}
        palette = 10=${base0B}
        palette = 11=${base0A}
        palette = 12=${base0D}
        palette = 13=${base0E}
        palette = 14=${base0C}
        palette = 15=${base07}
      '';
    };
  };
}
