{ lib, config, pkgs, ... }: {
  options.daedalus.home.notifications.mako.enable = lib.mkEnableOption "mako";

  config = lib.mkIf config.daedalus.home.notifications.mako.enable {
    services.mako = {
      enable = true;
      settings = {
        default-timeout = 5000; # 5 seconds
        ignore-timeout = false;

        on-notify = "exec mpv /run/current-system/sw/share/sounds/freedesktop/stereo/message.oga";
      };
    };
  };
}
