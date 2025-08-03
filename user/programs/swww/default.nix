{ config, lib, pkgs, ... }: {
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
}
