{ config, lib, ... }: let
  inherit (config.daedalus) username terminal;
in {
  flake-file.inputs.noctalia-shell = {
    url = "github:noctalia-dev/noctalia-shell";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  daedalus.lockscreen = lib.mkIf
    (config.daedalus.lockscreen.program == "noctalia-shell")
    { commandFor = _: "noctalia-shell ipc call lockScreen lock"; };

  flake.modules.nixos.noctalia = { lib, pkgs, inputs, config, ...}: {
    imports = [ inputs.noctalia-shell.nixosModules.default ];

    services.noctalia-shell.enable = true;

    hj.xdg.cache.files."noctalia/wallpapers.json" = {
      generator = lib.generators.toJSON { };
      value = {
        defaultWallpaper = config.stylix.image;
      };
    };

    hj.xdg.config.files."noctalia/settings.json" = {
      generator = lib.generators.toJSON { };
      value = {
        appLauncher = {
          customLaunchPrefix = "";
          customLaunchPrefixEnabled = false;
          enableClipboardHistory = true;
          pinnedExecs = [];
          position = "center";
          sortByMostUsed = true;
          terminalCommand = "kitty -e ";
          useApp2Unit = false;
        };
        audio = {
          cavaFrameRate = 30;
          mprisBlacklist = [];
          preferredPlayer = "";
          visualizerType = "mirrored";
          volumeOverdrive = false;
          volumeStep = 5;
        };
        bar = {
          backgroundOpacity = 1;
          capsuleOpacity = 1.0;
          density = "default";
          exclusive = true;
          floating = false;
          marginHorizontal = 0.25;
          marginVertical = 0.31;
          monitors = [];
          outerCorners = true;
          position = "bottom";
          showCapsule = true;
          widgets = {
            center = [
              {
                colorizeIcons = false;
                hideUnoccupied = false;
                id = "TaskbarGrouped";
                labelMode = "index";
                showLabelsOnlyWhenOccupied = false;
              }
            ];
            left = [
              {
                id = "SystemMonitor";
                showCpuTemp = true;
                showCpuUsage = true;
                showDiskUsage = false;
                showMemoryAsPercent = false;
                showMemoryUsage = true;
                showNetworkStats = true;
              }
              {
                hideMode = "hidden";
                id = "MediaMini";
                maxWidth = 145;
                scrollingMode = "hover";
                showAlbumArt = false;
                showVisualizer = true;
                useFixedWidth = false;
                visualizerType = "wave";
              }
              {
                colorizeIcons = false;
                hideMode = "hidden";
                id = "ActiveWindow";
                maxWidth = 145;
                scrollingMode = "hover";
                showIcon = true;
                useFixedWidth = false;
              }
            ];
            right = [
              {
                id = "ScreenRecorder";
              }
              {
                blacklist = [];
                colorizeIcons = false;
                id = "Tray";
              }
              {
                hideWhenZero = true;
                id = "NotificationHistory";
                showUnreadBadge = true;
              }
              {
                displayMode = "onhover";
                id = "WiFi";
              }
              {
                displayMode = "onhover";
                id = "Bluetooth";
              }
              {
                displayMode = "alwaysShow";
                id = "Battery";
                warningThreshold = 15;
              }
              {
                displayMode = "alwaysShow";
                id = "Volume";
              }
              {
                displayMode = "alwaysShow";
                id = "Brightness";
              }
              {
                customFont = "";
                formatHorizontal = "h:mm AP ddd, MMM dd";
                formatVertical = "HH mm - dd MM";
                id = "Clock";
                useCustomFont = false;
                usePrimaryColor = true;
              }
              {
                id = "PowerProfile";
              }
              {
                id = "KeepAwake";
              }
              {
                customIconPath = "";
                icon = "noctalia";
                id = "ControlCenter";
                useDistroLogo = true;
              }
              {
                id = "SessionMenu";
              }
            ];
          };
        };
        battery.chargingMode = 0;
        brightness = {
          brightnessStep = 5;
          enableDdcSupport = false;
          enforceMinimum = true;
        };
        colorSchemes = {
          darkMode = true;
          generateTemplatesForPredefined = true;
          manualSunrise = "06:30";
          manualSunset = "18:30";
          matugenSchemeType = "scheme-fruit-salad";
          predefinedScheme = "Noctalia (default)";
          schedulingMode = "off";
          useWallpaperColors = false;
        };
        controlCenter = {
          cards = [
            {
              enabled = true;
              id = "profile-card";
            }
            {
              enabled = true;
              id = "shortcuts-card";
            }
            {
              enabled = true;
              id = "audio-card";
            }
            {
              enabled = true;
              id = "weather-card";
            }
            {
              enabled = true;
              id = "media-sysmon-card";
            }
          ];
          position = "close_to_bar_button";
          shortcuts = {
            left = [
              {
                id = "WiFi";
              }
              {
                id = "Bluetooth";
              }
              {
                id = "ScreenRecorder";
              }
              {
                id = "WallpaperSelector";
              }
            ];
            right = [
              {
                id = "Notifications";
              }
              {
                id = "PowerProfile";
              }
              {
                id = "KeepAwake";
              }
              {
                id = "NightLight";
              }
            ];
          };
        };
        dock = {
          backgroundOpacity = 1.0;
          colorizeIcons = false;
          displayMode = "always_visible";
          enabled = false;
          floatingRatio = 1;
          monitors = [
      
          ];
          onlySameOutput = true;
          pinnedApps = [
      
          ];
          size = 1;
        };
        general = {
          animationDisabled = false;
          animationSpeed = 1;
          avatarImage = "/home/${username}/.face.icon";
          compactLockScreen = false;
          dimDesktop = true;
          enableShadows = true;
          forceBlackScreenCorners = false;
          language = "";
          lockOnSuspend = true;
          radiusRatio = 0.5;
          scaleRatio = 1;
          screenRadiusRatio = 0.58;
          shadowDirection = "bottom_right";
          shadowOffsetX = 2;
          shadowOffsetY = 3;
          showScreenCorners = true;
        };
        hooks = {
          darkModeChange = "";
          enabled = false;
          wallpaperChange = "";
        };
        location = {
          analogClockInCalendar = false;
          firstDayOfWeek = -1;
          name = "LOCATION";
          showCalendarEvents = true;
          showCalendarWeather = true;
          showWeekNumberInCalendar = false;
          use12hourFormat = true;
          useFahrenheit = true;
          weatherEnabled = true;
        };
        network.wifiEnabled = true;
        nightLight = {
          autoSchedule = true;
          dayTemp = "6500";
          enabled = false;
          forced = false;
          manualSunrise = "06:30";
          manualSunset = "18:30";
          nightTemp = "4000";
        };
        notifications = {
          backgroundOpacity = 1.0;
          criticalUrgencyDuration = 15;
          doNotDisturb = false;
          enabled = true;
          location = "top_right";
          lowUrgencyDuration = 3;
          monitors = [
            "eDP-1"
          ];
          normalUrgencyDuration = 8;
          overlayLayer = true;
          respectExpireTimeout = false;
        };
        osd = {
          autoHideMs = 2000;
          backgroundOpacity = 1.0;
          enabled = true;
          location = "top_right";
          monitors = [
      
          ];
          overlayLayer = true;
        };
        screenRecorder = {
          audioCodec = "opus";
          audioSource = "default_output";
          colorRange = "limited";
          directory = "/home/${username}/Videos/screen-recording/";
          frameRate = 60;
          quality = "very_high";
          showCursor = true;
          videoCodec = "h264";
          videoSource = "portal";
        };
        settingsVersion = 20;
        setupCompleted = true;
        ui = {
          fontDefault = config.stylix.fonts.sansSerif.name;
          fontDefaultScale = 1;
          fontFixed = config.stylix.fonts.monospace.name;
          fontFixedScale = 1;
          panelBackgroundOpacity = 1.0;
          panelsAttachedToBar = true;
          settingsPanelAttachToBar = false;
          tooltipsEnabled = true;
        };
        wallpaper = {
          defaultWallpaper = config.stylix.image;
          directory = "";
          enableMultiMonitorDirectories = false;
          enabled = false;
          fillColor = "#000000";
          fillMode = "crop";
          monitors = [];
          overviewEnabled = false;
          panelPosition = "follow_bar";
          randomEnabled = false;
          randomIntervalSec = 300;
          recursiveSearch = false;
          setWallpaperOnAllMonitors = true;
          transitionDuration = 1500;
          transitionEdgeSmoothness = 0.05;
          transitionType = "random";
        };
      };
    };

  hj.xdg.config.files."noctalia/colors.json" = {
    generator = lib.generators.toJSON { };
    value = with config.lib.stylix.colors.withHashtag; {
      mPrimary = base0D;
      mOnPrimary = base03;
      mSecondary = base0E;
      mOnSecondary = base03;
      mTertiary = base0B;
      mOnTertiary = base03;
      mError = base08;
      mOnError = base03;
      mSurface = base00;
      mOnSurface = base07;
      mSurfaceVariant = base02;
      mOnSurfaceVariant = base07;
      mOutline = base04;
      mShadow = base03;
      mHover = base0E;
      mOnHover = base03;
    };
  };

  sops.secrets.noctalia_location = { };

  # Hack to replace the string LOCATION with my actual location in
  # ~/.config/noctalia/settings.json while maintaining purity.
  # Reading from the secrets file during eval is impure.
  # This does make the file a normal file instead of a symlink to the store, but oh well.
  system.userActivationScripts = {
    addNoctaliaLocation = {
      text = ''
        run sed -i \
          "s/LOCATION/$(cat ${config.sops.secrets.noctalia_location.path})/" \
          /home/${username}/.config/noctalia/settings.json
        chown ${username}:users /home/${username}/.config/noctalia/settings.json
      '';
      deps = [];
    };
  };
};
}
