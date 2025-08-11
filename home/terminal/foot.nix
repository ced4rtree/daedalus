{ config, lib, pkgs, ... }: {
  options.daedalus.home.terminal.foot.enable = lib.mkEnableOption "foot";
  config.programs.foot = lib.mkIf config.daedalus.home.terminal.foot.enable {
    enable = true;
    settings = {
      main.term = "xterm-256color";
      scrollback.multiplier = 7.0;
    };
  };
}
