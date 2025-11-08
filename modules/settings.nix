{ lib, ... }: {
  options.daedalus = {
    username = lib.mkOption {
      description = "The primary user's username";
      default = "cedar";
      type = lib.types.str;
    };
    nvidia.enable = lib.mkEnableOption "nvidia";
    isLaptop = lib.mkEnableOption "isLaptop";
    bluetooth.enable = lib.mkEnableOption "bluetooth";
  };
}
