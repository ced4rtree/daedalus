{ lib, config, pkgs, ... }: {
  services.mako = {
    enable = true;
    settings = (builtins.readFile ./config.ini);
  };
}
