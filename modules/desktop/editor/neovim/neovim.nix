{ inputs, config, ... }: let
  # the config identifier gets shadowed later on
  topConfig = config;
in {
  perSystem = { pkgs, ... }: {
    packages.nvf = (inputs.nvf.lib.neovimConfiguration {
      inherit pkgs;
      modules = with topConfig.flake.modules.nvf; [
        keymaps
        options
        plugins
        theme
      ];
    }).neovim;
  };

  flake.modules.homeManager.neovim = { pkgs, inputs, lib, config, ... }: let
    system = pkgs.stdenv.hostPlatform.system;
  in {
    home.packages = [ topConfig.flake.packages.${system}.nvf ];
  };
}
