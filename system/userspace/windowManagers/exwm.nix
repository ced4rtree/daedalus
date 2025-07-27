{ config, pkgs, ... }: {
  imports = [
    ./xorg.nix
  ];

  services.xserver.windowManager = {
    session = pkgs.lib.singleton {
      name = "EXWM";
      start = ''
        emacs &
        waitPID=$!
      '';
    };
  };
}
