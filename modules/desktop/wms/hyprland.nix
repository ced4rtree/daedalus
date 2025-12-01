{
  flake.modules.nixos.hyprland = { pkgs, ... }: {
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
      ];
    };

    programs.hyprland.enable = true;
  };

  flake.modules.homeManager.hyprland = { config, lib, pkgs, ... }: {
    home.packages = with pkgs; [
      wl-clipboard
      wlr-randr
      egl-wayland
      grim
      sway-contrib.grimshot
      brightnessctl
    ];

    wayland.windowManager.hyprland = {
      enable = true;

      extraConfig = ''
        monitor = eDP-1,1920x1080@144,0x0,1
        monitor = HDMI-A-1,preferred,1920x0,1,mirror,eDP-1
      '';

      settings = {
        exec-once = [
          "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
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
          # kb_options = "ctrl:nocaps";
          kb_options = "caps:escape";
          kb_rules = "evdev";
          numlock_by_default = true;
          repeat_delay = 250;
          repeat_rate = 65;
          force_no_accel = false;
          float_switch_override_focus = 2;
          sensitivity = 0.2;

          follow_mouse = 1;
          mouse_refocus = 1;

          touchpad = {
            natural_scroll = true;
            scroll_factor = 0.4;
            disable_while_typing = true;
          };
        };

        cursor = {
          warp_on_change_workspace = 1;
        };

        ecosystem = {
          no_update_news = true;
          no_donation_nag = true;
        };

        general = {
          gaps_in = 5;
          gaps_out = 8;
          border_size = 2;
          "col.active_border" = let
            colors = config.lib.stylix.colors;
            # weird angle is equal to atan(1080/1920) to match the angle to the screen diagonal
          in lib.mkForce "rgba(${colors.blue}ff) rgba(${colors.magenta}ff) 29.357753542791272deg";
          resize_on_border = true;
          layout = "dwindle";
        };

        decoration = {
          rounding = 8;
          active_opacity = 1;
          inactive_opacity = 1;
          blur = {
            enabled = true;
            size = 4;
            passes = 2;
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
            animationDuration = "3";
            borderDuration = "3";
          in [
            "windows, 1, ${animationDuration}, md3_decel, popin 60%"
            "windowsIn, 1, ${animationDuration}, overshot, popin 60%"
            "windowsOut, 1, ${animationDuration}, md3_accel, popin"
            "border, 1, ${borderDuration}, default"
            "fade, 1, ${animationDuration}, md3_decel"
            "layersIn, 1, ${animationDuration}, menu_decel, slide top"
            "layersOut, 1, ${animationDuration}, menu_decel, slide top"
            "fadeLayersIn, 1, ${animationDuration}, menu_decel"
            "fadeLayersOut, 1, ${animationDuration}, menu_accel"
            "workspaces, 1, ${animationDuration}, hyprnostretch, slide"
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

        layerrule = [
          "animation slide right, notifications"
        ];

        bind = [
          # APP BINDS
          "ALT,return,exec,${config.daedalus.terminalCommand}"
          "ALT,E,exec,emacsclient -c -a 'emacs'"
          "ALTSHIFT,escape,exec,pkill Hyprland"

          # GENERAL WINDOWS OPERATIONS
          "ALT,space,togglefloating,"
          "ALT,G,togglegroup,"
          "ALT,C,changegroupactive,"
          "ALT,R,exec,fuzzel"
          "ALT,F,fullscreen,"
          "ALT,Escape,exec,noctalia-shell ipc call lockScreen lock"
          "CTRLALT,Escape,exec,noctalia-shell ipc call lockScreen lock; sleep 1; loginctl suspend"
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

          ",XF86AudioNext,exec,emacsclient -e '(emms-next)'"
          "ALT,right,exec,emacsclient -e '(emms-next)'"
          ",XF86AudioPrev,exec,emacsclient -e '(emms-previous)'"
          "ALT,left,exec,emacsclient -e '(emms-previous)'"
          "ALT,down,exec,emacsclient -e '(emms-pause)'"
          "ALT,up,exec,emacsclient -e '(emms-pause)'"
          ",XF86AudioPlay,exec,emacsclient -e '(emms-pause)'"

          # SCREENSHOTS
          "ALT,S,exec,grimshot save area ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"
          "ALTSHIFT,S,exec,grimshot save screen ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"

          # BRIGHTNESS CONTROL
          ",XF86MonBrightnessUp,exec,brightnessctl set +10%"
          ",XF86MonBrightnessDown,exec,brightnessctl set 10%-"
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

          # AUDIO CONTROL
          ",XF86AudioRaiseVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ +5%"
          ",XF86AudioLowerVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ -5%"
        ];
      };
    };
  };
}
