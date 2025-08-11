{ config, lib, pkgs, ... }: {
  options.daedalus.home.wallpaper.swww.enable = lib.mkEnableOption "swww";

  config = lib.mkIf config.daedalus.home.wallpaper.swww.enable {
    services.swww.enable = true;

    systemd.user.services.swww-image = {
      Unit.Description = "Set SWWW Wallpaper";
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.writeShellScript "swww-image" ''
        #!${pkgs.bash}/bin/bash
        ${pkgs.swww}/bin/swww img ${./wallpaper.gif}
      ''}";
      };
    };
  };
}
