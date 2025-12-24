{ inputs, config, lib, ... }: {
  flake-file.inputs = {
    hjem = {
      url = "github:feel-co/hjem";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  flake.modules.nixos.hjem = {
    imports = [
      inputs.hjem.nixosModules.default
      (lib.mkAliasOptionModule ["hj"] ["hjem" "users" config.daedalus.username])
    ];

    hjem.users.${config.daedalus.username} = {
      user = config.daedalus.username;
      directory = "/home/${config.daedalus.username}";
    };
  };
}
