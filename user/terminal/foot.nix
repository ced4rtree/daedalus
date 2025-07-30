{ config, lib, pkgs, ... }: {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main.term = "xterm-256color";
      scrollback.multiplier = 7.0;
    };
  };
}
