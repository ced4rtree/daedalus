{ config, lib, pkgs, ... }: {
  hardware.graphics = {
    enable = true;
    #driSupport = true;
    enable32Bit = true;
  };

  programs.xwayland.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
    powerManagement = {
      enable = false;
      finegrained = false;
    };
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.beta;

    prime = {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";

      sync.enable = true;
    };
  };
}
