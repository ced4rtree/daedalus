{ config, pkgs, lib, ... }: {
  services.displayManager.ly = {
    enable = true;
    settings = {
      animation = "doom";
      bigclock = "en";
    };
  };
}
