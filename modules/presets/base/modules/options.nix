{ lib, ... }: {
  options.daedalus = {
    username = lib.mkOption {
      description = "The primary user's username";
      default = "cedar";
      type = lib.types.str;
    };
  };
}
