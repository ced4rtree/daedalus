{
  description = "Bugger2 NixOS Configuration";

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
  };


  outputs = { self, nixpkgs, home-manager, nixvim, ... }@inputs:
    let
      lib = nixpkgs.lib;
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in {
      nixosConfigurations = {
        laptop = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./system
            home-manager.nixosModules.home-manager {
              home-manager = {
                sharedModules = [ nixvim.homeManagerModules.nixvim ];
                useGlobalPkgs = true;
                useUserPackages = true;
                users.cedar = import ./user;
              };
            }
          ];
        };
      };
    };
}
