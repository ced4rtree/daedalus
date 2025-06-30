{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    wl-clipboard
    wlr-randr
    egl-wayland
    grim
    sway-contrib.grimshot
    brightnessctl
    app2unit
    swaybg
  ];

  wayland.windowManager.hyprland = {
    enable = true;

    extraConfig = ''
      monitor = eDP-1,1920x1080@144,0x0,1
      monitor = HDMI-A-1,1920x1080@60,0x-1080,1
    '';
    
    settings = {
      exec-once = [
        "app2unit -- dbus-daemon --session --address=unix:path=$XDG_RUNTIME_DIR/bus"
        "app2unit -- dbus-update-activation-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
        "app2unit -- swaybg -i ~/.local/share/wallpapers/wallpaper.jpg"
        "app2unit -- quickshell"
      ];

      env = [
        "LIBVA_DRIVER_NAME,nvidia"
        "XDG_SESSION_TYPE,wayland"
        "GBM_BACKEND,nvidia-drm"
        "__GLX_VENDOR_LIBRARY_NAME,nvidia"
        "WLR_NO_HARDWARE_CURSORS,1"
      ];

      input = {
        kb_layout = "us";
        kb_options = "ctrl:nocaps";
        kb_rules = "evdev";
        numlock_by_default = true;
        follow_mouse = 1;
        repeat_delay = 250;
        repeat_rate = 65;
        force_no_accel = false;
        float_switch_override_focus = 2;
        sensitivity = 0.2;

        touchpad = {
          natural_scroll = true;
          scroll_factor = 0.4;
          disable_while_typing = true;
        };
      };

      general = {
        gaps_in = 8;
        gaps_out = 10;
        border_size = 2;
        "col.active_border" = "0xff8caaee";
        "col.inactive_border" = "0xdfca9ee6";
        resize_on_border = true;
        layout = "dwindle";
      };

      decoration = {
        rounding = 3;
        active_opacity = 1;
        inactive_opacity = 1;
        blur = {
          enabled = false;
          size = 2;
          passes = 1;
        };
      };

      # https://github.com/anotherhadi/nixy/blob/main/home/system/hyprland/animations.nix
      animations = {
        enabled = true;
        bezier = [
          "linear, 0, 0, 1, 1"
          "md3_standard, 0.2, 0, 0, 1"
          "md3_decel, 0.05, 0.7, 0.1, 1"
          "md3_accel, 0.3, 0, 0.8, 0.15"
          "overshot, 0.05, 0.9, 0.1, 1.1"
          "crazyshot, 0.1, 1.5, 0.76, 0.92"
          "hyprnostretch, 0.05, 0.9, 0.1, 1.0"
          "menu_decel, 0.1, 1, 0, 1"
          "menu_accel, 0.38, 0.04, 1, 0.07"
          "easeInOutCirc, 0.85, 0, 0.15, 1"
          "easeOutCirc, 0, 0.55, 0.45, 1"
          "easeOutExpo, 0.16, 1, 0.3, 1"
          "softAcDecel, 0.26, 0.26, 0.15, 1"
          "md2, 0.4, 0, 0.2, 1"
        ];

        animation = let
          animationDuration = "2.5";
          borderDuration = "6";
        in [
          "windows, 1, ${animationDuration}, md3_decel, popin 60%"
          "windowsIn, 1, ${animationDuration}, md3_decel, popin 60%"
          "windowsOut, 1, ${animationDuration}, md3_accel, popin 60%"
          "border, 1, ${borderDuration}, default"
          "fade, 1, ${animationDuration}, md3_decel"
          "layersIn, 1, ${animationDuration}, menu_decel, slide"
          "layersOut, 1, ${animationDuration}, menu_accel"
          "fadeLayersIn, 1, ${animationDuration}, menu_decel"
          "fadeLayersOut, 1, ${animationDuration}, menu_accel"
          "workspaces, 1, ${animationDuration}, menu_decel, slide"
          "specialWorkspace, 1, ${animationDuration}, md3_decel, slidevert"
        ];
      };
    
      misc = {
        disable_hyprland_logo = true;
        vrr = 1;
        mouse_move_enables_dpms = true;
        key_press_enables_dpms = true;
        enable_swallow = true;
        swallow_regex="[Ff][Oo][Oo][Tt]";
      };

      master = {
        new_on_top = true;
      };

      dwindle = {
        force_split = 2;
      };

      bind = [
        # APP BINDS
        "ALT,return,exec,app2unit -- footclient"
        "ALT,E,exec,app2unit -- emacsclient -c -a 'emacs'"
        "ALTSHIFT,escape,exec,app2unit -- pkill Hyprland"

        # GENERAL WINDOWS OPERATIONS
        "ALT,space,togglefloating,"
        "ALT,G,togglegroup,"
        "ALT,C,changegroupactive,"
        "ALT,R,global,quickshell:launcher"
        "ALT,T,pseudo,"
        "ALT,M,fullscreen,"
        "ALT,Escape,exec,swaylock -f -e -l -L -s fill"
        "CTRLALT,Escape,exec,app2unit -- swaylock -f -e -l -L -s fill; sleep 1; loginctl suspend"
        "ALTSHIFT,Q,killactive,"

        # FOCUS WORKSPACES
        "ALT,1,workspace,1"
        "ALT,2,workspace,2"
        "ALT,3,workspace,3"
        "ALT,4,workspace,4"
        "ALT,5,workspace,5"
        "ALT,6,workspace,6"
        "ALT,7,workspace,7"
        "ALT,8,workspace,8"
        "ALT,9,workspace,9"
        "ALT,0,workspace,10"

        # MOVING WINDOWS TO WS
        "ALTSHIFT,1,movetoworkspace,1"
        "ALTSHIFT,2,movetoworkspace,2"
        "ALTSHIFT,3,movetoworkspace,3"
        "ALTSHIFT,4,movetoworkspace,4"
        "ALTSHIFT,5,movetoworkspace,5"
        "ALTSHIFT,6,movetoworkspace,6"
        "ALTSHIFT,7,movetoworkspace,7"
        "ALTSHIFT,8,movetoworkspace,8"
        "ALTSHIFT,9,movetoworkspace,9"
        "ALTSHIFT,0,movetoworkspace,10"
        "ALTSHIFT,right,movetoworkspace,m+1"
        "ALTSHIFT,left,movetoworkspace,m-1"

        "ALT,H,movefocus,l"
        "ALT,L,movefocus,r"
        "ALT,K,movefocus,u"
        "ALT,J,movefocus,d"

        "ALTCONTROL,h,swapwindow,l"
        "ALTCONTROL,l,swapwindow,r"
        "ALTCONTROL,k,swapwindow,u"
        "ALTCONTROL,j,swapwindow,d"

        ",XF86AudioNext,exec,app2unit -- mpc next"
        ",XF86AudioPrev,exec,app2unit -- mpc prev"
        "ALT,down,exec,app2unit -- mpc toggle"
        "ALT,up,exec,app2unit -- mpc toggle"
        ",XF86AudioPlay,exec,app2unit -- mpc toggle"
        "ALT,right,exec,app2unit -- mpc next"
        "ALT,left,exec,app2unit -- mpc prev"

        # SCREENSHOTS
        "ALT,S,exec,app2unit -- grimshot save area ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"
        "ALTSHIFT,S,exec,app2unit -- grimshot save screen ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"

        # BRIGHTNESS CONTROL
        ",XF86MonBrightnessUp,global,quickshell:brightnessUp"
        ",XF86MonBrightnessDown,global,quickshell:brightnessDown"

        # AUDIO CONTROL
        ",XF86AudioRaiseVolume,global,quickshell:volumeUp"
        ",XF86AudioLowerVolume,global,quickshell:volumeDown"
      ];

      bindm = [
        # MOUSE WINDOW CONTROL
        "ALT,mouse:272,movewindow"
        "ALT,mouse:273,resizewindow"
      ];

      binde = [
        # WINDOW SIZE CONTROL
        "ALTSHIFT,H,resizeactive,-20 0"
        "ALTSHIFT,L,resizeactive,20 0"
        "ALTSHIFT,K,resizeactive,0 -20"
        "ALTSHIFT,J,resizeactive,0 20"
      ];
    };
  };
}
