{ lib, config, pkgs, ... }: {
  services.mako = {
    enable = true;
    settings = {
      default-timeout = 5000; # 5 seconds
      ignore-timeout = false;
      
      background-color = "#303446";
      text-color = "#c6d0f5";
      border-color = "#8caaee";
      progress-color = "over #414559";
      on-notify = "exec mpv /run/current-system/sw/share/sounds/freedesktop/stereo/message.oga";
      
      "urgency=high" = {
        border-color = "#ef9f76";
      };
    };
  };
}
