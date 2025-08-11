{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.desktop.batsignal.enable = lib.mkEnableOption "batsignal";
  config.services.batsignal.enable = config.daedalus.home.programs.desktop.batsignal.enable;
}
