{
  flake.modules.homeManager.noctalia = { lib, pkgs, inputs, ...}: {
    home.packages = with inputs; [
      quickshell.packages.${pkgs.system}.default
      noctalia-shell.packages.${pkgs.system}.default
    ] ++ (with pkgs; [
      # deps
      wlsunset
      brightnessctl
      ddcutil
      cliphist
      cava
    ]);

    systemd.user.services.noctalia-shell = {
      Unit = {
        Description = "Run Noctalia Shell";
        After = "graphical-session.target";
        BindsTo = "graphical-session.target";
        PartOf = "graphical-session.target";
        Requisite = "graphical-session.target";
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.writeShellScript "noctalia-shell" ''
          #!${pkgs.bash}/bin/bash
          ${lib.getExe inputs.noctalia-shell.packages.${pkgs.system}.default}
        ''}";
      };
    };
  };
}
