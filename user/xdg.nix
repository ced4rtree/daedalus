{ config, lib, pkgs, ... }: {
  environment.pathsToLink = [ "/share/xdg-desktop-portal" "/share/applications" ];
  xdg = {
    mime.enable = true;
    portal.enable = true;
    userDirs.enable = true;
  }
}
