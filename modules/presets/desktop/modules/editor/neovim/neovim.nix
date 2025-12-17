{ inputs, config, ... }: let
  # the config identifier gets shadowed later on
  topConfig = config;
in {
  flake-file.inputs.nvf = {
    url = "github:ced4rtree/nvf/feature/transparent-base16";
    inputs.nixpkgs.follows = "nixpkgs";
  };

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
