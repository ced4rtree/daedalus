{ config, inputs, ... }: {
  flake.modules.nixos.rofi = { pkgs, ... }: {
    hj.packages = [ config.flake.packages.${pkgs.stdenv.hostPlatform.system}.rofi ];
  };

  perSystem = { pkgs, ... }: {
    packages.rofi = (inputs.wrappers.wrapperModules.rofi.apply {
      inherit pkgs;
      settings = {
        font = "${config.flake.lib.stylix.fonts.sansSerif.name} ${toString config.flake.lib.stylix.fonts.sizes.desktop}";
      };
      theme = "${./theme.rasi}";
    }).wrapper;
  };
}
