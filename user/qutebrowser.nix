{ config, lib, pkgs, ... }: {
  home.packages = [ pkgs.python3Packages.adblock ];

  programs.qutebrowser = {
    enable = true;
    settings = {
      colors.webpage.darkmode.enabled = true;
    };
  };
}
