{
  flake.modules.nixos.nvidia = { config, lib, pkgs, ... }: {
    hardware.graphics.extraPackages = [ pkgs.nvidia-vaapi-driver ];

    environment.variables = {
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
    };

    services.xserver.videoDrivers = [ "nvidia" ];

    hardware.nvidia = {
      modesetting.enable = true;
      open = true;
      powerManagement = {
        enable = false;
        finegrained = false;
      };
      nvidiaSettings = true;
    };
  };

  flake.modules.nixos.nvidiaPrime = {
    hardware.nvidia.prime = {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";

      offload.enable = true;
    };
  };
}
