{
  flake.modules.nixos.bluetooth = { pkgs, lib, ... }: {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    hj.systemd.services.mpris-proxy = {
      description = "Proxy forwarding Bluetooth MIDI controls via MPRIS2 to control media players";
      bindsTo = [ "bluetooth.target" ];
      after = [ "bluetooth.target" ];

      wantedBy = [ "bluetooth.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = lib.getExe' pkgs.bluez "mpris-proxy";
      };
    };
  };
}
