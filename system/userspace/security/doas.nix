{ config, lib, pkgs, ... }: {
  options.daedalus.security.doas.enable = lib.mkEnableOption "doas";

  config.security.doas = lib.mkIf config.daedalus.security.doas.enable {
    enable = true;
    extraRules = [{
      users = ["cedar"];
      keepEnv = true;
      persist = true;
    }];
  };
}
