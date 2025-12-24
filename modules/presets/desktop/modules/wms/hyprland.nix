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

  perSystem = { pkgs, ... }: let
    modKey = "SUPER";
    hyprland-config = pkgs.writeText "hyprland-config" (with config.flake.lib.stylix.colors; ''
      # STARTUP
      exec-once = ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY HYPRLAND_INSTANCE_SIGNATURE WAYLAND_DISPLAY XDG_CURRENT_DESKTOP && systemctl --user stop hyprland-session.target && systemctl --user start hyprland-session.target

      env = LIBVA_DRIVER_NAME,nvidia
      env = XDG_SESSION_TYPE,wayland
      env = GBM_BACKEND,nvidia-drm
      env = __GLX_VENDOR_LIBRARY_NAME,nvidia
      env = WLR_NO_HARDWARE_CURSORS,1

      # MONITOR CONFIG
      monitor = eDP-1,1920x1080@144,0x0,1
      monitor = HDMI-A-1,preferred,1920x0,1,mirror,eDP-1
      
      # INPUT
      input {
        touchpad {
          disable_while_typing = true
          natural_scroll = true
          scroll_factor = 0.400000
        }

        repeat_delay = 250
        repeat_rate = 65
        numlock_by_default = true

        float_switch_override_focus = 2
        follow_mouse = 1
        force_no_accel = false
        mouse_refocus = 1
        sensitivity = 0.200000

        kb_layout = us
        #kb_options = caps:escape
        kb_options = ctrl:nocaps
        kb_rules = evdev
      }
      
      # FANCY COLORS & WHATNOT
      animations {
        bezier = linear, 0, 0, 1, 1
        bezier = md3_standard, 0.2, 0, 0, 1
        bezier = md3_decel, 0.05, 0.7, 0.1, 1
        bezier = md3_accel, 0.3, 0, 0.8, 0.15
        bezier = overshot, 0.05, 0.9, 0.1, 1.1
        bezier = crazyshot, 0.1, 1.5, 0.76, 0.92
        bezier = hyprnostretch, 0.05, 0.9, 0.1, 1.0
        bezier = menu_decel, 0.1, 1, 0, 1
        bezier = menu_accel, 0.38, 0.04, 1, 0.07
        bezier = easeInOutCirc, 0.85, 0, 0.15, 1
        bezier = easeOutCirc, 0, 0.55, 0.45, 1
        bezier = easeOutExpo, 0.16, 1, 0.3, 1
        bezier = softAcDecel, 0.26, 0.26, 0.15, 1
        bezier = md2, 0.4, 0, 0.2, 1

        animation = windows, 1, 3, md3_decel, popin 60%
        animation = windowsIn, 1, 3, overshot, popin 60%
        animation = windowsOut, 1, 3, md3_accel, popin
        animation = border, 1, 3, default
        animation = fade, 1, 3, md3_decel
        animation = layersIn, 1, 3, menu_decel, slide top
        animation = layersOut, 1, 3, menu_decel, slide top
        animation = fadeLayersIn, 1, 3, menu_decel
        animation = fadeLayersOut, 1, 3, menu_accel
        animation = workspaces, 1, 3, hyprnostretch, slide
        animation = specialWorkspace, 1, 3, md3_decel, slidevert
        enabled = true
      }
      
      cursor {
        warp_on_change_workspace = 1
      }

      decoration {
        blur {
          enabled = true
          passes = 2
          size = 4
        }
      
        active_opacity = 1
        inactive_opacity = 1
        rounding = 8
      }
      
      ecosystem {
        no_donation_nag = true
        no_update_news = true
      }
      
      general {
        border_size = 2

        # that long decimal is equal to arctan(9/16), i.e. angle of the hypotenuse a 16:9 monitor
        col.active_border = rgb(${blue}) rgb(${magenta}) 29.357753542791272deg
        col.inactive_border = rgb(3f444a)

        gaps_in = 5
        gaps_out = 8
        layout = dwindle
        resize_on_border = true
      }
      
      group {
        groupbar {
          col.active = rgb(${blue})
          col.inactive = rgb(${base03})
          text_color = rgb(${base05})
        }
        col.border_active = rgb(${blue})
        col.border_inactive = rgb(${base03})
        col.border_locked_active = rgb(${cyan})
      }
      
      master {
        new_on_top = true
      }
      
      misc {
        background_color = rgb(${base00})
        disable_hyprland_logo = true
        enable_swallow = true
        key_press_enables_dpms = true
        mouse_move_enables_dpms = true
        swallow_regex=[Ff][Oo][Oo][Tt]
        vrr = 1
      }

      # KEYBINDINGS
      bind = ${modKey},return,exec,${config.daedalus.terminal.command}
      bind = ${modKey},E,exec,emacsclient -c -a 'emacs'
      bind = ${modKey},R,exec,fuzzel

      # brightness
      bind=,XF86MonBrightnessUp,exec,${pkgs.brightnessctl}/bin/brightnessctl set +10%
      bind=,XF86MonBrightnessDown,exec,${pkgs.brightnessctl}/bin/brightnessctl set 10%-

      # audio
      binde=,XF86AudioRaiseVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ +5%
      binde=,XF86AudioLowerVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ -5%

      bind = ${modKey}SHIFT,escape,exec,hyprctl dispatch exit
      bind = ${modKey},space,togglefloating,
      bind = ${modKey},G,togglegroup,
      bind = ${modKey},C,changegroupactive,
      bind = ${modKey},F,fullscreen,
      bind = ${modKey},Escape,exec,${config.daedalus.lockscreen.command}
      bind = CTRL${modKey},Escape,exec,${config.daedalus.lockscreen.command}; sleep 1; loginctl suspend
      bind = ${modKey}SHIFT,Q,killactive,

      # music control
      bind=,XF86AudioNext,exec,emacsclient -e '(emms-next)'
      bind = ${modKey},right,exec,emacsclient -e '(emms-next)'
      bind=,XF86AudioPrev,exec,emacsclient -e '(emms-previous)'
      bind = ${modKey},left,exec,emacsclient -e '(emms-previous)'
      bind = ${modKey},down,exec,emacsclient -e '(emms-pause)'
      bind = ${modKey},up,exec,emacsclient -e '(emms-pause)'
      bind=,XF86AudioPlay,exec,emacsclient -e '(emms-pause)'

      # screenshot stuff
      bind = ${modKey},S,exec,grimshot save area ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png
      bind = ${modKey}SHIFT,S,exec,grimshot save screen ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png

      layerrule = animation slide right, notifications

      bind = ${modKey},1,workspace,1
      bind = ${modKey},2,workspace,2
      bind = ${modKey},3,workspace,3
      bind = ${modKey},4,workspace,4
      bind = ${modKey},5,workspace,5
      bind = ${modKey},6,workspace,6
      bind = ${modKey},7,workspace,7
      bind = ${modKey},8,workspace,8
      bind = ${modKey},9,workspace,9
      bind = ${modKey},0,workspace,10
      bind = ${modKey}SHIFT,1,movetoworkspace,1
      bind = ${modKey}SHIFT,2,movetoworkspace,2
      bind = ${modKey}SHIFT,3,movetoworkspace,3
      bind = ${modKey}SHIFT,4,movetoworkspace,4
      bind = ${modKey}SHIFT,5,movetoworkspace,5
      bind = ${modKey}SHIFT,6,movetoworkspace,6
      bind = ${modKey}SHIFT,7,movetoworkspace,7
      bind = ${modKey}SHIFT,8,movetoworkspace,8
      bind = ${modKey}SHIFT,9,movetoworkspace,9
      bind = ${modKey}SHIFT,0,movetoworkspace,10
      bind = ${modKey}SHIFT,right,movetoworkspace,m+1
      bind = ${modKey}SHIFT,left,movetoworkspace,m-1
      bind = ${modKey},H,movefocus,l
      bind = ${modKey},L,movefocus,r
      bind = ${modKey},K,movefocus,u
      bind = ${modKey},J,movefocus,d
      bind = ${modKey}CONTROL,h,swapwindow,l
      bind = ${modKey}CONTROL,l,swapwindow,r
      bind = ${modKey}CONTROL,k,swapwindow,u
      bind = ${modKey}CONTROL,j,swapwindow,d
      binde = ${modKey}SHIFT,H,resizeactive,-20 0
      binde = ${modKey}SHIFT,L,resizeactive,20 0
      binde = ${modKey}SHIFT,K,resizeactive,0 -20
      binde = ${modKey}SHIFT,J,resizeactive,0 20

      bindm = ${modKey},mouse:272,movewindow
      bindm = ${modKey},mouse:273,resizewindow
    '');
  in {
    packages.hyprland = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.hyprland;
      flags = {
        "-c" = "${hyprland-config}";
      };
    };
  };
}
