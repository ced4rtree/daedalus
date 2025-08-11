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
          sha256 = "sha256:1l9ddc8q0lfcwwnx5drnqkbqzm0b9wbzli9fjbs2ccqakydsfhj2";
        }))
      ];
      config.allowUnfree = true;
    };

    generateHost = hostname: lib.nixosSystem {
      inherit system pkgs;
      specialArgs = {
        mylib = (import ./lib { inherit lib; });
        inherit hostname;
      };
      modules = [
        ./hosts/${hostname}
        ./system
        stylix.nixosModules.stylix
        (import ./common/stylix.nix false)
      ];
    };
  in {
    nixosConfigurations =
      ./hosts
        |> builtins.readDir
        |> builtins.attrNames
        |> map (host: {
          name = host;
          value = generateHost host;
        })
        |> builtins.listToAttrs;

    homeConfigurations = {
      "cedar" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          mylib = (import ./lib { inherit lib; });
        };
        modules = [
          nixvim.homeModules.nixvim
          stylix.homeModules.stylix
          (import ./common/stylix.nix true)
          ./home
        ];
      };
    };
  };
}
