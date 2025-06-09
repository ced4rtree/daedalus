{ config, pkgs, ... }: {
  services.xserver.enable = true;
  services.libinput.enable = true;
  services.libinput.touchpad.naturalScrolling = true;
  services.xserver.displayManager.startx.enable = true;
}
