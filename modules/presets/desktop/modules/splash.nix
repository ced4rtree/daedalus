{
  flake-file.inputs.dark-text = {
    url = "github:vimjoyer/dark-text";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.modules.nixos.splash = { lib, inputs, pkgs, ... }: {
    system.userActivationScripts = {
      splash = {
        text = "${lib.getExe inputs.dark-text.packages.${pkgs.stdenv.hostPlatform.system}.default} --text 'NixOS Rebuilt' --duration 3000";
        deps = [];
      };
    };
  };

  flake.modules.homeManager.splash = { config, lib, inputs, pkgs, ... }: {
    home.activation = {
      splash = config.lib.dag.entryAfter ["writeBoundary"] ''
        run ${lib.getExe inputs.dark-text.packages.${pkgs.stdenv.hostPlatform.system}.default} --duration 3000 --text "Home-Manager Rebuilt"
      '';
    };
  };
}
