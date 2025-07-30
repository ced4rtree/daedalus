{ lib, config, pkgs, ... }: {
  services.mako = {
    enable = true;
    settings = {
      default-timeout = 5000; # 5 seconds
      ignore-timeout = false;
      
      on-notify = "exec mpv /run/current-system/sw/share/sounds/freedesktop/stereo/message.oga";
    };
  };
}
