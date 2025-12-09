{
  flake.modules.nixos.hostOptions = { lib, ... }: {
    options.daedalus = {
      efi.enable = lib.mkEnableOption "efi";
      isDesktop = lib.mkEnableOption "desktop";
    };
  };
}
