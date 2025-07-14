{ config, pkgs, lib, ... }: {
  home.packages = with pkgs; [
    cava
    fftw
    iniparser
  ];

  services.playerctld.enable = true;
  
  programs.waybar = {
    enable = true;
    style =
      (builtins.readFile ./frappe.css)
      + (builtins.readFile ./style.css);

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
          "idle_inhibitor"
          "mpris"
          "cava"
          "pulseaudio"
          "custom/weather"
          "network"
          "cpu"
          "memory"
          "temperature"
          "backlight"
          "battery"
          "power-profiles-daemon"
          "clock"
          "tray"
        ];

        "idle_inhibitor" = {
          format = "{icon}";
          format-icons = {
            "activated" = " ";
            "deactivated" = " ";
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
          format = "{usage}%  ";
          tooltip = false;
        };

        "memory" = {
          format = "{}%  ";
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

        "battery" = {
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{capacity}% {icon} ";
          format-charging = "{capacity}% 󱐋";
          format-plugged = "{capacity}% ";
          format-alt = "{time} {icon}";
          format-icons = ["" "" "" "" ""];
        };

        "power-profiles-daemon" = {
          format-icons = {
            power-saver = "󰌪 ";
            balanced = " ";
            performance = "󰓅 ";
          };
        };

        "network" = {
          format-wifi = "{signalStrength}%  ";
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
            headphone = " ";
            hands-free = " ";
            headset = " ";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" " "];
          };
          on-click = "pavucontrol";
        };

        "mpd" = {
          format = "{artist} - {title}  ";
          format-paused = "{artist} - {title} ";
        };

        "mpris" = {
          format = "{dynamic} {player_icon}";
          format-paused = "{dynamic} {status_icon}";
          dynamic-order = ["artist" "title" "position" "length"];
          player-icons = {
            default = "▶";
            emms = "🎵";
          };
          status-icons = {
            paused = "⏸";
          };
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
            propagatedBuildInputs = [
              (pkgs.python3.withPackages (pythonPackages: with pythonPackages; [
                pyquery
              ]))
            ];
            dontUnpack = true;
            installPhase = "install -Dm755 ${./weather.py} $out/bin/weather.py";
          };
        in {
          exec = "${weather-script}/bin/weather.py";
          restart-interval = 300;
          return-type = "json";
          on-click = "xdg-open https://weather.com/en-IN/weather/today/l/$location_id";
        };
      };
    };
  };
}
