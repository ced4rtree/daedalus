{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.desktop.xdg.enable = lib.mkEnableOption "xdg";

  config = lib.mkIf config.daedalus.home.programs.desktop.xdg.enable {
    xdg = {
      mime.enable = true;
      userDirs.enable = true;
    };
  };
}
