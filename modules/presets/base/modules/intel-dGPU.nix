{
  flake.modules.nixos.intel-dGPU = { pkgs, ... }: {
    hardware.graphics.extraPackages = [ pkgs.intel-media-driver ];
  };
}
