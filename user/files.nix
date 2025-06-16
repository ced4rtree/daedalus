{ config, lib, pkgs, ... }: {
  home.file.".local/share/wallpapers/wallpaper.jpg".source = ../files/.local/share/wallpapers/wallpaper.jpg;
  home.file.".face".source = ../files/.face;
  home.file.".gnupg/gpg.conf".source = ../files/.gnupg/gpg.conf;
}
