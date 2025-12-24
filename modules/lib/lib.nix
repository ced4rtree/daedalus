{ lib, ... }: {
  options.flake.lib = lib.mkOption {
    description = "Library functions used throughout the flake";
    type = lib.types.attrs;
    default = { };
  };
}
