{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.internet.nyxt.enable = lib.mkEnableOption "nyxt";

  config = lib.mkIf config.daedalus.home.programs.internet.nyxt.enable {
    programs.nyxt = {
      enable = true;
      config = builtins.readFile ./config.lisp;
    };
  };
}
