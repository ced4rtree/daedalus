{
  flake.modules.nixos.batsignal = { pkgs, lib, ... }: {
    hj.systemd.services.batsignal = {
      description = "batsignal - battery monitor daemon";
      after = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${lib.getExe pkgs.batsignal}";
        Restart = "on-failure";
        RestartSec = 1;
      };

      wantedBy = [ "graphical-session.target" ];
    };
  };
}
