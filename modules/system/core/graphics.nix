{
  flake.modules.nixos.graphics = { config, lib, pkgs, ... }: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = [ pkgs.libva ];
    };
  };
}
