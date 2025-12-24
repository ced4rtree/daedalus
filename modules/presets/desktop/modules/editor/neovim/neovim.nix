{ inputs, config, ... }: let
  # the config identifier gets shadowed later on
  topConfig = config;
in {
  flake-file.inputs.nvf = {
    url = "github:ced4rtree/nvf/feature/transparent-base16";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.modules.nixos.neovim = { pkgs, inputs, lib, config, ... }: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    hj.packages = [ topConfig.flake.packages.${system}.nvf ];
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
}
