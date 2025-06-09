{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    wl-clipboard
    wlr-randr
    swaybg
    egl-wayland
    grim
    sway-contrib.grimshot
    brightnessctl
  ];

  wayland.windowManager.hyprland = {
    enable = true;

    extraConfig = "monitor = eDP-1,1920x1080@144,0x0,1";
    
    settings = {
      exec-once = [
        "dbus-daemon --session --address=unix:path=$XDG_RUNTIME_DIR/bus"
        "swaybg -i ~/.local/share/wallpapers/wallpaper.jpg"
        "dbus-update-activation-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
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
          animationDuration = "4.5";
          borderDuration = "10";
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
        "SUPER,return,exec,foot"
        "SUPER,E,exec,emacsclient -c -a 'emacs'"
        "SUPERSHIFT,escape,exec,pkill Hyprland"

        # GENERAL WINDOWS OPERATIONS
        "SUPER,space,togglefloating,"
        "SUPER,G,togglegroup,"
        "SUPER,C,changegroupactive,"
        "SUPER,R,exec,rofi -show drun -terminal foot"
        "SUPER,T,pseudo,"
        "SUPER,F,fullscreen,"
        "SUPER,Escape,exec,swaylock -f -e -l -L -s fill"
        "CTRLSUPER,Escape,exec,swaylock -f -e -l -L -s fill; sleep 1; loginctl suspend"
        "SUPERSHIFT,Q,killactive,"
        "SUPERSHIFT,T,exec,~/.config/hypr/scripts/switchLayout"

        # FOCUS WORKSPACES
        "SUPER,1,workspace,1"
        "SUPER,2,workspace,2"
        "SUPER,3,workspace,3"
        "SUPER,4,workspace,4"
        "SUPER,5,workspace,5"
        "SUPER,6,workspace,6"
        "SUPER,7,workspace,7"
        "SUPER,8,workspace,8"
        "SUPER,9,workspace,9"
        "SUPER,0,workspace,10"

        # MOVING WINDOWS TO WS
        "SUPERSHIFT,1,movetoworkspace,1"
        "SUPERSHIFT,2,movetoworkspace,2"
        "SUPERSHIFT,3,movetoworkspace,3"
        "SUPERSHIFT,4,movetoworkspace,4"
        "SUPERSHIFT,5,movetoworkspace,5"
        "SUPERSHIFT,6,movetoworkspace,6"
        "SUPERSHIFT,7,movetoworkspace,7"
        "SUPERSHIFT,8,movetoworkspace,8"
        "SUPERSHIFT,9,movetoworkspace,9"
        "SUPERSHIFT,0,movetoworkspace,10"
        "SUPERSHIFT,right,movetoworkspace,m+1"
        "SUPERSHIFT,left,movetoworkspace,m-1"

        "SUPER,H,movefocus,l"
        "SUPER,L,movefocus,r"
        "SUPER,K,movefocus,u"
        "SUPER,J,movefocus,d"

        "SUPER,return,layoutmsg,swapwithmaster master"

        ",XF86AudioNext,exec,mpc next"
        ",XF86AudioPrev,exec,mpc prev"
        "SUPER,down,exec,mpc toggle"
        "SUPER,up,exec,mpc toggle"
        ",XF86AudioPlay,exec,mpc toggle"
        "SUPER,right,exec,mpc next"
        "SUPER,left,exec,mpc prev"

        # SCREENSHOTS
        "SUPER,S,exec,grimshot save area ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"
        "SUPERSHIFT,S,exec,grimshot save screen ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"

        # BRIGHTNESS CONTROL
        ",XF86MonBrightnessUp,exec,brightnessctl set +5%"
        ",XF86MonBrightnessDown,exec,brightnessctl set 5%-"
      ];

      bindm = [
        "SUPER,mouse:272,movewindow"
        "SUPER,mouse:273,resizewindow"
      ];

      binde = [
        "SUPERSHIFT,H,resizeactive,-20 0"
        "SUPERSHIFT,L,resizeactive,20 0"
        "SUPERSHIFT,K,resizeactive,0 -20"
        "SUPERSHIFT,J,resizeactive,0 20"

        ",XF86AudioRaiseVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ +5%"
        ",XF86AudioLowerVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ -5%"
      ];
    };
  };
}
