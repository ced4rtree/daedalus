{ config, lib, pkgs, ... }: {
  home.file.".local/share/wallpapers/wallpaper.jpg".source = ./wallpaper.jpg;
  home.file.".face".source = ./.face;
}
