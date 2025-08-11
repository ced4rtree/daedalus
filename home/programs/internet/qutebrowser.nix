{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.internet.qutebrowser.enable = lib.mkEnableOption "qutebrowser";

  config = lib.mkIf config.daedalus.home.programs.internet.qutebrowser.enable {
    home.packages = [ pkgs.python3Packages.adblock ];

    programs.qutebrowser = {
      enable = true;
      settings = {
        colors.webpage.darkmode.enabled = true;
      };
    };
  };
}
