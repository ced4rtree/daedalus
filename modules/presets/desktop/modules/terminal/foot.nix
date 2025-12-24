{ inputs, config, ... }: {
  flake.modules.nixos.foot = { pkgs, ... }: {
    hj.packages = [ config.flake.packages.${pkgs.stdenv.hostPlatform.system}.foot ];
  };

  perSystem = { pkgs, ... }: {
    packages.foot = (inputs.wrappers.wrapperModules.foot.apply {
      inherit pkgs;
      settings = let
        inherit (config.flake.lib) stylix;
      in {
        main = {
          term = "xterm-256color";

          font = "${stylix.fonts.monospace.name}:size=${toString stylix.fonts.sizes.terminal}";
          dpi-aware = "no";

          include = toString (stylix.createColorsFrom pkgs {
            templateRepo = inputs.stylix.inputs.tinted-foot;
          });
        };

        colors.alpha = toString stylix.opacity.terminal;

        scrollback.multiplier = 7.0;
      };
    }).wrapper;
  };
}
