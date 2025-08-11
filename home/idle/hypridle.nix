{ config, lib, pkgs, ... }: {
  options.daedalus.home.idle.hypridle.enable = lib.mkEnableOption "hypridle";

  config = lib.mkIf config.daedalus.home.idle.hypridle.enable {
    home.packages = [
      pkgs.brightnessctl
    ];

    services.hypridle = {
      enable = true;
      settings = {
        general = {
          lock_cmd = "pidof hyprlock || hyprlock";      # avoid starting multiple hyprlock instances.
          before_sleep_cmd = "loginctl lock-session";   # lock before suspend.
          after_sleep_cmd = "hyprctl dispatch dpms on"; # to avoid having to press a key twice to turn on the display.
        };

        listener = [
          # decrease monitor brightness
          {
            timeout = 150; # 2.5 min
            on-timeout = ''brightnessctl -s set $(awk "BEGIN {print $(brightnessctl get)*0.2}")'';
            on-resume = "brightnessctl -r"; # monitor backlight restore.
          }

          # turn off keyboard backlight and dim screen to black after 5 minutes
          { 
            timeout = 300; # 5min
            on-timeout = "brightnessctl -sd rgb:kbd_backlight set 0"; # turn off keyboard backlight.
            on-resume = "brightnessctl -rd rgb:kbd_backlight"; # turn on keyboard backlight.
          }
          {
            timeout = 300;
            on-timeout = "brightnessctl set 10"; # (0 is bad on OLED)
            on-resume = "brightnessctl -r";
          }

          # lock session after 10 minutes
          {
            timeout = 600;
            on-timeout = "loginctl lock-session";
          }

          # turn screen completely off 30 seconds after locking
          {
            timeout = 630;
            on-timeout = "hyprctl dispatch dpms off"; # screen off when timeout has passed
            on-resume = "hyprctl dispatch dpms on"; # screen on when activity is detected after timeout has fired.
          }

          # suspend pc after 30 minutes
          {
            timeout = 1800;
            on-timeout = "systemctl suspend";
          }
        ];
      };
    };
  };
}
