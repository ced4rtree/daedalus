{
  flake.modules.homeManager.mpris = { config, lib, pkgs, ... }: {
    services.mpris-proxy.enable = true;
  };
}
