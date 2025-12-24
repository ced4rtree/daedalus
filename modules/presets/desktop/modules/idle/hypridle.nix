{ config, inputs, ... }: {
  flake.modules.nixos.hypridle = { pkgs, lib, ... }: let
    hypridle = config.flake.packages.${pkgs.stdenv.hostPlatform.system}.hypridle;
    systemdTarget = "graphical-session.target";
  in {
    hj.packages = [ hypridle ];

    hj.systemd.services.hypridle = {
      wantedBy = [ systemdTarget ];

      description = "hypridle";
      after = [ systemdTarget ];
      partOf = [ systemdTarget ];
      restartTriggers = [ hypridle ];

      unitConfig = {
        ConditionEnvironment = "WAYLAND_DISPLAY";
      };

      serviceConfig = {
        ExecStart = "${lib.getExe hypridle}";
        Restart = "always";
        RestartSec = "10";
      };
    };
  };

  perSystem = { pkgs, lib, ... }: {
    packages.hypridle = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.hypridle;

      flags."-c" = "${pkgs.writeText "hypridle.conf" (config.flake.lib.generators.toHyprlang { } {
        general = {
          lock_cmd = config.daedalus.lockscreen.command;
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
      })}";
    };
  };
}
