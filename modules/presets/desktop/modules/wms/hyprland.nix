{ inputs, config, ... }: {
  flake.modules.nixos.hyprland = { inputs, pkgs, ... }: {
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
      ];
    };

    programs.hyprland = {
      enable = true;
      package = config.flake.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    };

    systemd.user.targets.hyprland-session = {
      description = "Hyprland compositor session";
      documentation = [ "man:systemd.special(7)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [
        "graphical-session-pre.target"
      ];
      after = [ "graphical-session-pre.target" ];
    };

    hj.packages = with pkgs; [
      wl-clipboard
      wlr-randr
      egl-wayland
      grim
      sway-contrib.grimshot
      brightnessctl
    ];
  };

  perSystem = { pkgs, system, ... }: let
    modKey = "SUPER";
    inherit (config.flake.lib.stylix) colors;
  in {
    packages.hyprland = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.hyprland;
      flags = {
        "-c" = "${pkgs.writeText "hyprland-config" (config.flake.lib.generators.toHyprlang { } {
          exec-once = "${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY HYPRLAND_INSTANCE_SIGNATURE WAYLAND_DISPLAY XDG_CURRENT_DESKTOP && systemctl --user stop hyprland-session.target && systemctl --user start hyprland-session.target";
          env = [
            "LIBVA_DRIVER_NAME,nvidia"
            "XDG_SESSION_TYPE,wayland"
            "GBM_BACKEND,nvidia-drm"
            "__GLX_VENDOR_LIBRARY_NAME,nvidia"
            "WLR_NO_HARDWARE_CURSORS,1"
          ];

          monitor = [
            "eDP-1,1920x1080@144,0x0,1"
            "HDMI-A-1,preferred,1920x0,1,mirror,eDP-1"
          ];
          
          input = {
            touchpad = {
              disable_while_typing = true;
              natural_scroll = true;
              scroll_factor = 0.400000;
            };

            repeat_delay = 250;
            repeat_rate = 65;
            numlock_by_default = true;

            float_switch_override_focus = 2;
            follow_mouse = 1;
            force_no_accel = false;
            mouse_refocus = 1;
            sensitivity = 0.2;

            kb_layout = "us";
            #kb_options = caps:escape;
            kb_options = "ctrl:nocaps";
            kb_rules = "evdev";
          };
          
          # FANCY COLORS & WHATNOT
          animations = {
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

            animation = [
              "windows, 1, 3, md3_decel, popin 60%"
              "windowsIn, 1, 3, overshot, popin 60%"
              "windowsOut, 1, 3, md3_accel, popin"
              "border, 1, 3, default"
              "fade, 1, 3, md3_decel"
              "layersIn, 1, 3, menu_decel, slide top"
              "layersOut, 1, 3, menu_decel, slide top"
              "fadeLayersIn, 1, 3, menu_decel"
              "fadeLayersOut, 1, 3, menu_accel"
              "workspaces, 1, 3, hyprnostretch, slide"
              "specialWorkspace, 1, 3, md3_decel, slidevert"
            ];

            enabled = true;
          };
          
          cursor = {
            warp_on_change_workspace = 1;
          };

          decoration = {
            blur = {
              enabled = true;
              passes = 2;
              size = 4;
            };
            
            active_opacity = 1;
            inactive_opacity = 1;
            rounding = 0; # was 8
          };
          
          ecosystem = {
            no_donation_nag = true;
            no_update_news = true;
          };
          
          general = {
            border_size = 2;

            # that long decimal is equal to arctan(9/16), i.e. angle of the hypotenuse a 16:9 monitor
            col.active_border = "rgb(${colors.blue}) rgb(${colors.magenta}) 29.357753542791272deg";
            col.inactive_border = "rgb(3f444a)";

            gaps_in = 5;
            gaps_out = 8;
            layout = "dwindle";
            resize_on_border = true;
          };
          
          group = {
            groupbar = {
              col.active = "rgb(${colors.blue})";
              col.inactive = "rgb(${colors.base03})";
              text_color = "rgb(${colors.base05})";
            };
            col.border_active = "rgb(${colors.blue})";
            col.border_inactive = "rgb(${colors.base03})";
            col.border_locked_active = "rgb(${colors.cyan})";
          };
          
          master = {
            new_on_top = true;
          };
          
          misc = {
            background_color = "rgb(${colors.base00})";
            disable_hyprland_logo = true;
            enable_swallow = true;
            key_press_enables_dpms = true;
            mouse_move_enables_dpms = true;
            swallow_regex="[Ff][Oo][Oo][Tt]";
            vrr = 1;
          };

          # KEYBINDINGS
          binde = [
            # brightness
            ",XF86MonBrightnessUp,exec,${pkgs.brightnessctl}/bin/brightnessctl set +10%"
            ",XF86MonBrightnessDown,exec,${pkgs.brightnessctl}/bin/brightnessctl set 10%-"

            # audio
            ",XF86AudioRaiseVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ +5%"
            ",XF86AudioLowerVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ -5%"

            # window resizing
            "${modKey}SHIFT,H,resizeactive,-20 0"
            "${modKey}SHIFT,L,resizeactive,20 0"
            "${modKey}SHIFT,K,resizeactive,0 -20"
            "${modKey}SHIFT,J,resizeactive,0 20"
          ];

          bind = [
            "${modKey},return,exec,${config.daedalus.terminal.commandFor system}"
            "${modKey},E,exec,emacsclient -c -a 'emacs'"
            "${modKey},R,exec,fuzzel"

            "${modKey}SHIFT,escape,exec,hyprctl dispatch exit"
            "${modKey},space,togglefloating,"
            "${modKey},G,togglegroup,"
            "${modKey},C,changegroupactive,"
            "${modKey},F,fullscreen,"
            "${modKey},Escape,exec,${config.daedalus.lockscreen.commandFor system}"
            "CTRL${modKey},Escape,exec,${config.daedalus.lockscreen.commandFor system}; sleep 1; loginctl suspend"
            "${modKey}SHIFT,Q,killactive,"

            # music control
            ",XF86AudioNext,exec,emacsclient -e '(emms-next)'"
            "${modKey},right,exec,emacsclient -e '(emms-next)'"
            ",XF86AudioPrev,exec,emacsclient -e '(emms-previous)'"
            "${modKey},left,exec,emacsclient -e '(emms-previous)'"
            "${modKey},down,exec,emacsclient -e '(emms-pause)'"
            "${modKey},up,exec,emacsclient -e '(emms-pause)'"
            ",XF86AudioPlay,exec,emacsclient -e '(emms-pause)'"

            # screenshot stuff
            "${modKey},S,exec,grimshot save area ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"
            "${modKey}SHIFT,S,exec,grimshot save screen ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"
            
            "${modKey},1,workspace,1"
            "${modKey},2,workspace,2"
            "${modKey},3,workspace,3"
            "${modKey},4,workspace,4"
            "${modKey},5,workspace,5"
            "${modKey},6,workspace,6"
            "${modKey},7,workspace,7"
            "${modKey},8,workspace,8"
            "${modKey},9,workspace,9"
            "${modKey},0,workspace,10"
            "${modKey}SHIFT,1,movetoworkspace,1"
            "${modKey}SHIFT,2,movetoworkspace,2"
            "${modKey}SHIFT,3,movetoworkspace,3"
            "${modKey}SHIFT,4,movetoworkspace,4"
            "${modKey}SHIFT,5,movetoworkspace,5"
            "${modKey}SHIFT,6,movetoworkspace,6"
            "${modKey}SHIFT,7,movetoworkspace,7"
            "${modKey}SHIFT,8,movetoworkspace,8"
            "${modKey}SHIFT,9,movetoworkspace,9"
            "${modKey}SHIFT,0,movetoworkspace,10"
            "${modKey}SHIFT,right,movetoworkspace,m+1"
            "${modKey}SHIFT,left,movetoworkspace,m-1"
            "${modKey},H,movefocus,l"
            "${modKey},L,movefocus,r"
            "${modKey},K,movefocus,u"
            "${modKey},J,movefocus,d"
            "${modKey}CONTROL,h,swapwindow,l"
            "${modKey}CONTROL,l,swapwindow,r"
            "${modKey}CONTROL,k,swapwindow,u"
            "${modKey}CONTROL,j,swapwindow,d"
          ];

          bindm = [
            "${modKey},mouse:272,movewindow"
            "${modKey},mouse:273,resizewindow"
          ];

          layerrule = [ "animation slide right, notifications" ];
        })}";
      };
    };
  };
}
