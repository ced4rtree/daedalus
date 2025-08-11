{ config, lib, pkgs, ... }: {
  # NOT meant to be enabled/disabled explicitly, but rather by individual wms
  options.daedalus.wm.xorg.enable = lib.mkEnableOption "xorg";

  config.services = lib.mkIf config.daedalus.wm.xorg.enable {
    xserver.enable = true;
    libinput.enable = true;
    libinput.touchpad.naturalScrolling = true;
    xserver.displayManager.startx.enable = true;
  };
}
