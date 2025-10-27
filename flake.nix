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

    nixcord = {
      url = "github:KaylorBen/nixcord";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    quickshell = {
      url = "github:outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia-shell = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.quickshell.follows = "quickshell";
    };

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dark-text = {
      url = "github:vimjoyer/dark-text";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixvim,
    stylix,
    quickshell,
    noctalia-shell,
    ...
  }@inputs: let
    lib = nixpkgs.lib;
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (import (builtins.fetchTarball {
          url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
          sha256 = "sha256:0nqy59yynz39rdqf49axxp8yjksixayccbr5q1wnv6vfapb9m2ik";
        }))
      ];
      config.allowUnfree = true;
    };

    generateHost = hostname: lib.nixosSystem {
      inherit system pkgs;
      specialArgs = {
        mylib = (import ./lib { inherit lib; });
        inherit inputs hostname;
      };
      modules = [
        ./hosts/${hostname}
        ./hosts/host-spec.nix
        ./system
        stylix.nixosModules.stylix
        (import ./common/stylix.nix false)
      ];
    };
  in {
    nixosConfigurations =
      ./hosts
        |> builtins.readDir
        |> lib.filterAttrs (file: type: type == "directory")
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
          inherit inputs;
        };
        modules = [
          stylix.homeModules.stylix
          (import ./common/stylix.nix true)
          ./home
        ];
      };
    };
  };
}
