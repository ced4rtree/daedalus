{ lib, ... }: {
  options.daedalus = {
    username = lib.mkOption {
      description = "The primary user's username";
      default = "cedar";
      type = lib.types.str;
    };
    isLaptop = lib.mkOption {
      description = "Whether or not the host is a laptop";
      type = lib.types.boolean;
    };
  };
}
