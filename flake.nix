{
  description = "Ced4rtree NixOS Configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };


  outputs = { self, nixpkgs, home-manager, nixvim, stylix, ... }@inputs: let
    lib = nixpkgs.lib;
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (import (builtins.fetchTarball {
          url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
          sha256 = "sha256:02sf8pk9dd5jfxz9ad560i5r3x2bhn40kkwvkq2rmrii30k07w3p";
        }))
      ];
      config.allowUnfree = true;
    };
  in {
    nixosConfigurations = {
      laptop = lib.nixosSystem {
        inherit system;
        modules = [
          stylix.nixosModules.stylix
          (import ./common/stylix.nix false)
          ./system
        ];
      };
    };

    homeConfigurations = {
      "cedar" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          nixvim.homeManagerModules.nixvim
          stylix.homeModules.stylix
          (import ./common/stylix.nix true)
          ./user
        ];
      };
    };
  };
}
