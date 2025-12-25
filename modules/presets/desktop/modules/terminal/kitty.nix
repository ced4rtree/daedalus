{ inputs, config, lib, ... }: {
  daedalus.terminal = lib.mkIf
    (config.daedalus.terminal.program == "kitty")
    { commandFor = system: "${lib.getExe config.flake.packages.${system}.kitty}"; };

  flake.modules.nixos.kitty = { pkgs, ... }: {
    hj.packages = [ config.flake.packages.${pkgs.stdenv.hostPlatform.system}.kitty ];
  };

  perSystem = { pkgs, lib, ... }: {
    packages.kitty = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.kitty;
      flags = let
        inherit (config.flake.lib) stylix;
        # somewhat hacky way of getting around module level pkgs acting weird in lib.stylix
        colors = stylix.createColorsFrom pkgs;
      in {
        "-c" = "${pkgs.writeText "kitty-config" ''
          font_family ${config.flake.lib.stylix.fonts.monospace.name}
          font_size ${toString config.flake.lib.stylix.fonts.sizes.terminal}

          background_opacity ${toString config.flake.lib.stylix.opacity.terminal}

          cursor_trail 1
          enable_audio_bell no
          visual_bell_colors ${config.flake.lib.stylix.colors.withHashtag.red}
          visual_bell_duration 0.5

          include ${colors {
            templateRepo = inputs.stylix.inputs.tinted-kitty;
            target = "base16";
          }}
        ''}";
      };
    };
  };
}
