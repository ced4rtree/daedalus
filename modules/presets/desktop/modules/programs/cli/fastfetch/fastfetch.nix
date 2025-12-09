{
  flake.modules.homeManager.fastfetch = { config, ... }: {
    programs.fastfetch = {
      enable = true;
      settings = {
        logo = {
          source = "${./logo.txt}";
          type = "file";
          color = with config.lib.stylix.colors.withHashtag; {
            "1" = red;
            "2" = green;
            "3" = blue;
            "4" = cyan;
            "5" = magenta;
            "6" = brown;
          };
        };
        modules = [
          "title"
          "separator"
          "os"
          "host"
          "kernel"
          "uptime"
          "packages"
          "shell"
          "display"
          "de"
          "wm"
          "terminal"
          "cpu"
          "gpu"
          "memory"
          "disk"
          "break"
          "colors"
          # "Break"
          # "Battery"
          # "BIOS"
          # "Bluetooth"
          # "Bluetooth Radio"
          # "Board"
          # "Boot Manager"
          # "Brightness"
          # "BTRFS"
          # "Camera"
          # "Chassis"
          # "CPU"
          # "CPU Cache"
          # "CPU Usage"
          # "Colors"
          # "Command"
          # "Cursor"
          # "Custom"
          # "Date Time"
          # "Display"
          # "Disk"
          # "DiskIO"
          # "Desktop Environment"
          # "DNS"
          # "Editor"
          # "Font"
          # "Gamepad"
          # "GPU"
          # "Host"
          # "Icons"
          # "Init System"
          # "Kernel"
          # "Keyboard"
          # "Login Manager"
          # "Local IP"
          # "Loadavg"
          # "Locale"
          # "Media"
          # "Memory"
          # "Mouse"
          # "Monitor"
          # "NetIO"
          # "OpenCL"
          # "OpenGL"
          # "Operating System"
          # "Packages"
          # "Physical Disk"
          # "Physical Memory"
          # "Player"
          # "Power Adapter"
          # "Processes"
          # "Public IP"
          # "Separator"
          # "Shell"
          # "Sound"
          # "Swap"
          # "Terminal"
          # "Terminal Font"
          # "Terminal Size"
          # "Terminal Theme"
          # "Theme"
          # "Title"
          # "TPM"
          # "Users"
          # "Uptime"
          # "Version"
          # "Vulkan"
          # "Wallpaper"
          # "Weather"
          # "Wi-Fi"
          # "Window Manager"
          # "WM Theme"
          # "Zpool"
        ];
      };
    };
  };
}
