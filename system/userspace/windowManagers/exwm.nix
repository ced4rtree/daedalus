{ config, lib, pkgs, ... }: {
  options.daedalus.wm.exwm.enable = lib.mkEnableOption "exwm";

  config = lib.mkIf config.daedalus.wm.exwm.enable {
    daedalus.wm.xorg.enable = true;
    services.xserver.windowManager = {
      session = pkgs.lib.singleton {
        name = "EXWM";
        start = ''
        emacs &
        waitPID=$!
      '';
      };
    };
  };
}
