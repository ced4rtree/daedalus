{
  flake.modules.nixos.gnome-polkit = { pkgs, ... }: {
    security.polkit.enable = true;

    # taken from home manager source code
    systemd.user.services.polkit-gnome = {
      enable = true;

      description = "GNOME PolicyKit Agent";
      after = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      };
    };
  };
}
