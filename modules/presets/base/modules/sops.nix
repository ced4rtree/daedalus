{ inputs, config, ... }: let
  sops = {
    defaultSopsFile = ../../../../secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/home/${config.daedalus.username}/.config/sops/age/keys.txt";
  };
in {
  flake-file.inputs.sops-nix.url = "github:Mic92/sops-nix";

  flake.modules.nixos.sops = {
    imports = [ inputs.sops-nix.nixosModules.sops ];
    inherit sops;
  };

  flake.modules.homeManager.sops = {
    imports = [ inputs.sops-nix.homeManagerModules.sops ];
    inherit sops;
  };
}
