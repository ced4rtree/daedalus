{
  flake.modules.nixos.swww = { pkgs, lib, ... }: {
    hj.systemd.services.swww = {
      name = "swww-daemon";
      script = "${lib.getExe' pkgs.swww "swww-daemon"}";

      environment = {
        PATH = "$PATH:${lib.makeBinPath [ pkgs.swww ]}";
      };

      wantedBy = "graphical-session.target";
      after = "graphical-session.target";
      partOf = "graphical-session.target";
    };

    hj.systemd.services.swww-image = {
      description = "Set SWWW Wallpaper";
      script = "${lib.getExe' pkgs.swww "swww-image"}";
      scriptArgs = "img ${./wallpaper.gif}";

      after = "swww-daemon.service";
      restartTriggers = [
        "${./wallpaper.gif}"
      ];
      serviceConfig = {
        Type = "oneshot";
      };
    };
  };
}
