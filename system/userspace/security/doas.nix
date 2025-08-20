{ config, lib, pkgs, ... }: {
  options.daedalus.security.doas.enable = lib.mkEnableOption "doas";

  config = lib.mkIf config.daedalus.security.doas.enable {
    security.doas = {
      enable = true;
      extraRules = [{
        users = ["cedar"];
        keepEnv = true;
        persist = true;
      }];
    };

    environment.systemPackages = [
      pkgs.doas-sudo-shim
    ];
  };
}
