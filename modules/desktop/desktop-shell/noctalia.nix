{ config, ... }: let
  username = config.daedalus.username;
in {
  flake.modules.homeManager.noctalia = { lib, pkgs, inputs, config, ...}: {
    imports = [ inputs.noctalia-shell.homeModules.default ];

    programs.noctalia-shell = {
      enable = true;
      settings = {
        settingsVersion = 20;
        setupCompleted = true;
        bar = {
          position = "bottom";
          backgroundOpacity = 1;
          monitors = [ ];
          density = "default";
          showCapsule = true;
          floating = false;
          marginVertical = 0.31;
          marginHorizontal = 0.25;
          outerCorners = true;
          exclusive = true;
          widgets = {
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
                id = "MediaMini";
                hideMode = "hidden";
                maxWidth = 145;
                scrollingMode = "hover";
                showAlbumArt = false;
                showVisualizer = true;
                useFixedWidth = false;
                visualizerType = "wave";
              }
              {
                id = "ActiveWindow";
                colorizeIcons = false;
                hideMode = "hidden";
                maxWidth = 145;
                scrollingMode = "hover";
                showIcon = true;
                useFixedWidth = false;
              }
            ];
            center = [
                {
                  id = "Workspace";
                  hideUnoccupied = false;
                  labelMode = "none";
                }
                {
                  id = "Taskbar";
                  colorizeIcons = false;
                  hideMode = "hidden";
                  onlyActiveWorkspaces = false;
                  onlySameOutput = true;
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
        general = {
          avatarImage = "/home/${username}/.face.icon";
          dimDesktop = true;
          showScreenCorners = true;
          forceBlackScreenCorners = false;
          scaleRatio = 1;
          radiusRatio = 0.5;
          screenRadiusRatio = 0.58;
          animationSpeed = 1;
          animationDisabled = false;
          compactLockScreen = false;
          lockOnSuspend = true;
          enableShadows = true;
          shadowDirection = "bottom_right";
          shadowOffsetX = 2;
          shadowOffsetY = 3;
          language = "";
        };
        ui = {
          fontDefault = config.stylix.fonts.sansSerif.name;
          fontFixed = config.stylix.fonts.monospace.name;
          fontDefaultScale = 1;
          fontFixedScale = 1;
          tooltipsEnabled = true;
          panelsAttachedToBar = true;
          settingsPanelAttachToBar = false;
        };
        location = {
          name = "LOCATION";
          weatherEnabled = true;
          useFahrenheit = true;
          use12hourFormat = true;
          showWeekNumberInCalendar = false;
          showCalendarEvents = true;
          showCalendarWeather = true;
          analogClockInCalendar = false;
          firstDayOfWeek = -1;
        };
        screenRecorder = {
          directory = "/home/${username}/Videos";
          frameRate = 60;
          audioCodec = "opus";
          videoCodec = "h264";
          quality = "very_high";
          colorRange = "limited";
          showCursor = true;
          audioSource = "default_output";
          videoSource = "portal";
        };
        wallpaper = {
          enabled = false;
          overviewEnabled = false;
          directory = builtins.dirOf config.stylix.image;
          enableMultiMonitorDirectories = false;
          recursiveSearch = false;
          setWallpaperOnAllMonitors = true;
          defaultWallpaper = config.stylix.image;
          fillMode = "crop";
          fillColor = "#000000";
          randomEnabled = false;
          randomIntervalSec = 300;
          transitionDuration = 1500;
          transitionType = "random";
          transitionEdgeSmoothness = 0.05;
          monitors = [ ];
          panelPosition = "follow_bar";
        };
        appLauncher = {
          enableClipboardHistory = true;
          position = "center";
          backgroundOpacity = 0.75;
          pinnedExecs = [ ];
          useApp2Unit = false;
          sortByMostUsed = true;
          terminalCommand = "kitty -e";
          customLaunchPrefixEnabled = false;
          customLaunchPrefix = "";
        };
        controlCenter = {
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
        };
        dock = {
          enabled = false;
          displayMode = "always_visible";
          backgroundOpacity = 1;
          floatingRatio = 1;
          size = 1;
          onlySameOutput = true;
          monitors = [ ];
          pinnedApps = [ ];
          colorizeIcons = false;
        };
        network = {
          wifiEnabled = true;
        };
        notifications = {
          enabled = true;
          doNotDisturb = false;
          monitors = [ "eDP-1" ];
          location = "top_right";
          overlayLayer = true;
          backgroundOpacity = 1;
          respectExpireTimeout = false;
          lowUrgencyDuration = 3;
          normalUrgencyDuration = 8;
          criticalUrgencyDuration = 15;
        };
        osd = {
          enabled = true;
          location = "top_right";
          monitors = [ ];
          autoHideMs = 2000;
          overlayLayer = true;
        };
        audio = {
          volumeStep = 5;
          volumeOverdrive = false;
          cavaFrameRate = 30;
          visualizerType = "mirrored";
          mprisBlacklist = [ ];
          preferredPlayer = "";
        };
        brightness = {
          brightnessStep = 5;
          enforceMinimum = true;
          enableDdcSupport = false;
        };
        colorSchemes = {
          useWallpaperColors = false;
          predefinedScheme = "Noctalia (default)";
          darkMode = true;
          schedulingMode = "off";
          manualSunrise = "06:30";
          manualSunset = "18:30";
          matugenSchemeType = "scheme-fruit-salad";
          generateTemplatesForPredefined = true;
        };
        nightLight = {
          enabled = false;
          forced = false;
          autoSchedule = true;
          nightTemp = "4000";
          dayTemp = "6500";
          manualSunrise = "06:30";
          manualSunset = "18:30";
        };
        hooks = {
          enabled = false;
          wallpaperChange = "";
          darkModeChange = "";
        };
        battery = {
          chargingMode = 0;
        };
      };

      colors = with config.lib.stylix.colors.withHashtag; {
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
    home.activation = {
      addNoctaliaLocation = config.lib.dag.entryAfter ["reloadSystemd"] ''
        run sed -i "s/LOCATION/$(cat ${config.sops.secrets.noctalia_location.path})/" /home/${username}/.config/noctalia/settings.json
      '';
    };

    xdg.configFile."noctalia/settings.json".force = true;
    xdg.configFile."noctalia/colors.json".force = true;

    programs.noctalia-shell.systemd.enable = true;
  };
}
