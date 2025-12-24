{ config, inputs, ... }: {
  flake.modules.nixos.nyxt = { pkgs, ... }: {
    hj.packages = [ config.flake.packages.${pkgs.stdenv.hostPlatform.system}.nyxt ];
  };

  perSystem = { pkgs, ... }: {
    packages.nyxt = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.nyxt;

      flags = {
        "--init" = "${./config.lisp}";
        "--auto-config" = "${./auto-config.lisp}";
      };
    };
  };
}
