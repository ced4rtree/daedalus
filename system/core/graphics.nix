{ config, lib, pkgs, ... }: {
  options.daedalus.graphics.enable = lib.mkEnableOption "graphics";

  config = lib.mkIf config.daedalus.graphics.enable {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };

    environment.sessionVariables.NIXOS_OZONE_WL =
      if config.daedalus.wm.xorg.enable
      then "1"
      else "0";

    services.xserver.videoDrivers = [ "nvidia" ];

    hardware.nvidia = {
      modesetting.enable = true;
      open = true;
      powerManagement = {
        enable = false;
        finegrained = false;
      };
      nvidiaSettings = true;

      prime = {
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";

        offload.enable = true;
      };
    };
  };
}
