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

    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };


  outputs = { self, nixpkgs, home-manager, nixvim, quickshell, ... }@inputs:
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
                extraSpecialArgs = { inherit inputs; };
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
