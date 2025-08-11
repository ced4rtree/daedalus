{ config, lib, pkgs, ... }: {
  options.daedalus.plymouth.enable = lib.mkEnableOption "plymouth";

  config.boot = lib.mkIf config.daedalus.plymouth.enable {
    plymouth.enable = true;

    # silent boot
    consoleLogLevel = 3;
    initrd.verbose = false;
    kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "rd.systemd.show_status=auto"
    ];
  };
}
