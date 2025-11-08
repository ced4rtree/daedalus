{
  flake.modules.nixos.graphics = { config, lib, pkgs, ... }: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = lib.optional
        config.daedalus.host.nvidia.enable
        pkgs.nvidia-vaapi-driver;
    };

    environment.variables = lib.mkIf config.daedalus.host.nvidia.enable {
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
    };

    environment.sessionVariables.NIXOS_OZONE_WL =
      if config.daedalus.wm.xorg.enable
      then "1"
      else "0";

    services.xserver.videoDrivers = [ "nvidia" ];

    hardware.nvidia = lib.mkIf config.daedalus.host.nvidia.enable {
      modesetting.enable = true;
      open = true;
      powerManagement = {
        enable = false;
        finegrained = false;
      };
      nvidiaSettings = true;

      prime = lib.mkIf config.daedalus.host.isLaptop {
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";

        offload.enable = true;
      };
    };
  };
}
