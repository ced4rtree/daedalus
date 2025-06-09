{ config, lib, pkgs, ... }: {
  home.file.".local/share/wallpapers/wallpaper.jpg".source = ../files/.local/share/wallpapers/wallpaper.jpg;
  home.file.".face.icon".source = ../files/.face.icon;
  home.file.".gnupg/gpg.conf".source = ../files/.gnupg/gpg.conf;
}
