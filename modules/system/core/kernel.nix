{
  flake.modules.nixos.kernel = { pkgs, ... }: {
    boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;
  };
}
