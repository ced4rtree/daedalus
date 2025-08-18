{ config, pkgs, lib, ... }: {
  options.daedalus.home.bar.waybar.enable = lib.mkEnableOption "waybar";

  config = lib.mkIf config.daedalus.home.bar.waybar.enable {
    services.playerctld.enable = true;

    stylix.targets.waybar = {
      addCss = false;
      font = "sansSerif";
    };

    home.packages = with pkgs; [
      cava
      fftw
      iniparser
    ];

    programs.waybar = {
      enable = true;
      style = let
        colors = config.lib.stylix.colors.withHashtag;
      in ''
        /* base08-base0F & base12-base17 */
        @define-color red ${colors.red};
        @define-color orange ${colors.orange};
        @define-color yellow ${colors.yellow};
        @define-color green ${colors.green};
        @define-color cyan ${colors.cyan};
        @define-color blue ${colors.blue};
        @define-color magenta ${colors.magenta};
        @define-color brown ${colors.brown};
        @define-color bright-red ${colors.red};
        @define-color bright-yellow ${colors.bright-yellow};
        @define-color bright-green ${colors.bright-green};
        @define-color bright-cyan ${colors.bright-cyan};
        @define-color bright-blue ${colors.bright-blue};
        @define-color bright-magenta ${colors.bright-magenta};

        /* base00-base07 */
        @define-color base ${colors.base00};
        @define-color mantle ${colors.base01};
        @define-color surface0 ${colors.base02};
        @define-color surface1 ${colors.base03};
        @define-color surface2 ${colors.base04};
        @define-color text ${colors.base05};
      '' + builtins.readFile ./style.css;

      systemd = {
        enable = true;
        target = "graphical-session.target";
      };

      settings = {
        mainBar = {
          layer = "top";
          position = "bottom";
          height = 10;

          modules-left = ["hyprland/workspaces" "hyprland/window"];
          modules-right = [
            "temperature"
            "network"
            "backlight"
            "battery"
            "custom/weather"
            "mpris"
            "cava"
            "pulseaudio"
            "clock"
            "idle_inhibitor"
            "power-profiles-daemon"
            "tray"
          ];

          "idle_inhibitor" = {
            format = "{icon}";
            format-icons = {
              "activated" = "";
              "deactivated" = "";
            };
          };

          "tray" = {
            icon-size = 21;
            spacing = 10;
          };

          "clock" = {
            format = "{:%B %d, %Y %I:%M%p}";
            tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          };

          "cpu" = {
            format = "{usage}% ";
            tooltip = false;
          };

          "memory" = {
            format = "{}% ";
          };

          "temperature" = {
            hwmon-path = "/sys/class/hwmon/hwmon6/temp1_input";
            critical-threshold = 80;
            format = "{temperatureC}°C {icon}";
            format-icons = ["" "" ""];
          };

          "backlight" = {
            format = "{percent}% {icon} ";
            format-icons = ["" "" "" "" "" "" "" "" ""];
          };

          "battery" = let
            format = "{capacity}% {icon}";
          in {
            states = {
              warning = 30;
              critical = 15;
            };
            format = format;
            format-charging = "{capacity}% 󱐋";
            format-plugged = "{capacity}% ";
            format-alt = format;
            format-icons = ["" "" "" "" ""];
            interval = 5;
          };

          "power-profiles-daemon" = {
            format-icons = {
              power-saver = "󰌪";
              balanced = "";
              performance = "󰓅";
            };
          };

          "network" = {
            format-wifi = "{signalStrength}% ";
            format-ethernet = "{ipaddr}/{cidr}";
            tooltip-format = "{ifname}: {essid} ({ipaddr}/{cidr})";
            format-linked = "{ifname} (No IP)";
            format-disconnected = "Disconnected ⚠";
            # format-alt = "{ifname}: {ipaddr}/{cidr}";
            on-click = "foot -e nmtui";
          };

          "pulseaudio" = {
            format = "{volume}% {icon}";
            format-bluetooth = "{volume}% {icon}";
            format-bluetooth-muted = " {icon}";
            format-muted = "";
            format-source = "{volume}%";
            format-source-muted = "";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = ["" "" ""];
            };
            on-click = "pavucontrol";
          };

          "mpd" = {
            format = "{artist} - {title} ";
            format-paused = "{artist} - {title} ";
          };

          "mpris" = {
            format = "{dynamic} {player_icon}";
            format-paused = "{dynamic} {status_icon}";
            dynamic-order = ["artist" "title"];
            player-icons = {
              default = "▶";
              emms = "";
            };
            status-icons = {
              paused = "⏸";
            };
            dynamic-len = 50;
          };

          "cava" = {
            framerate = 30;
            sensitivity = 1;
            bars = 14;
            lower_cutoff_freq = 50;
            higher_cutoff_freq = 10000;
            hide_on_silence = true;
            method = "pipewire";
            source = "auto";
            stereo = true;
            reverse = false;
            bar_delimiter = 0;
            monstercat = false;
            waves = false;
            noise_reduction = 0.77;
            input_delay = 2;
            format-icons = ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" ];
            actions = {
              on-click-right = "mode";
            };
          };

          "custom/weather" = let
            weather-script = pkgs.stdenv.mkDerivation {
              name = "weather-script";
              dontUnpack = true;
              installPhase = "install -Dm755 ${./weather.sh} $out/bin/weather.sh";
            };
          in {
            exec = "${weather-script}/bin/weather.sh";
            restart-interval = (3 * 60 * 60); # every 3 hours
            return-type = "text";
            on-click = "xdg-open https://weather.com/en-IN/weather/today/l/$location_id";
          };
        };
      };
    };
  };
}
