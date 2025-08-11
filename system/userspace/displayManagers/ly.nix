{ config, pkgs, lib, ... }: {
  services.displayManager.ly = lib.mkIf (config.daedalus.displayManager == "ly") {
    enable = true;
    settings = {
      animation = "doom";
      bigclock = "en";
    };
  };
}
