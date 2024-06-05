{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    wlr-randr
    wmenu
    sway
    i3blocks
    foot
    wl-clipboard
    swaylock
  ];
}
