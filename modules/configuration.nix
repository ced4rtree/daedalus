{ config, lib, inputs, ... }: let
  system = "x86_64-linux";

  pkgs = import inputs.nixpkgs {
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
    alexandria = mkNixos "alexandria";
  };

  flake.homeConfigurations = {
    ${config.daedalus.username} = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = with config.flake.modules.homeManager; [
        base
        desktop
      ];
    };
  };
}
