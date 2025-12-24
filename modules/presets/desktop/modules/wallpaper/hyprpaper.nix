{ config, inputs, ... }: {
  perSystem = { pkgs, ... }: {
    packages.hyprpaper = let
      hyprpaperConfig = pkgs.writeText "hyprpaper-config" ''
        preload = ${config.flake.lib.stylix.image}
        wallpaper = , ${config.flake.lib.stylix.image}
      '';
    in inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.hyprpaper;
      flags = {
        "-c" = "${hyprpaperConfig}";
      };
    };
  };

  flake.modules.nixos.hyprpaper = { pkgs, lib, ... }: let
    flakePkgs = config.flake.packages.${pkgs.stdenv.hostPlatform.system};
  in {
    systemd.user.services.hyprpaper = {
      description = "hyprpaper";
      script = "${lib.getExe flakePkgs.hyprpaper}";

      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      restartTriggers = [
        "${config.flake.lib.stylix.image}"
      ];
    };
  };
}
