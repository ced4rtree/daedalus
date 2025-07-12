{ config, pkgs, lib, ... }: {
  imports = [
    ./weather.nix
  ];
  
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
          "mpd"
          "custom/weather"
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "temperature"
          "backlight"
          "battery"
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

        "network" = {
          format-wifi = "{signalStrength}%  ";
          format-ethernet = "{ipaddr}/{cidr}";
          tooltip-format = "{essid}";
          format-linked = "{ifname} (No IP)";
          format-disconnected = "Disconnected ⚠";
          format-alt = "{ifname}: {ipaddr}/{cidr}";
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

        "custom/weather" = {
          exec = "weather.py";
        "mpd" = {
          format = "{artist} - {title}  ";
          format-paused = "{artist} - {title} ";
        };

          restart-interval = 300;
          return-type = "json";
          on-click = "xdg-open https://weather.com/en-IN/weather/today/l/$location_id";
        };
      };
    };
  };
}
