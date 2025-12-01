{ config, lib, inputs, ... }: let
  system = "x86_64-linux";

  pkgs = import inputs.nixpkgs {
    overlays = [
      (import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
        sha256 = "sha256:0s54smhaq6rw660qdwya07i8cpvziw11j9qax86sk2fhdlj3dnzj";
      }))
    ];
    config.allowUnfree = true;
    inherit system;
    hostPlatform = system;
  };

  mkNixos = name:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      inherit pkgs;
      specialArgs = {
        inherit inputs;
      };
      modules = [
        config.flake.modules.nixos.${name}
        config.flake.modules.nixos.base
        {
          networking.hostName = lib.mkDefault name;
          system.stateVersion = "25.05";
        }
      ];
    };
in {
  systems = [ system ];

  flake.nixosConfigurations = {
    icarus = mkNixos "icarus";
  };

  flake.homeConfigurations = {
    ${config.daedalus.username} = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        config.flake.modules.homeManager.base
      ];
    };
  };
}
