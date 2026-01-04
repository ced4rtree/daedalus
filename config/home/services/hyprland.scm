(define-module (config home services hyprland)
  #:use-module (config util colors)
  #:use-module (gnu home services)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (nongnu packages nvidia)
  #:use-module (srfi srfi-1)
  #:export (home-hyprland-service-type
            my/hyprland-config))

(define my/hyprland-config
  `((exec-once . ("dbus-update-activation-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
                  "noctalia-shell"
                  "emacs --daemon"))
    (env . ("LIBVA_DRIVER_NAME,nvidia"
            "XDG_SESSION_TYPE,wayland"
            "__GLX_VENDOR_LIBRARY_NAME,nvidia"
            "WLR_NO_HARDWARE_CURSORS,1"
            "XDG_CURRENT_DESKTOP,Hyprland"
            "XDG_SESSION_DESKTOP,Hyprland"))
    (monitor . ("eDP-1,1920x1080@144,0x0,1"
                "HDMI-A-1,preferred,auto,2.5,mirror,eDP-1"))
    (input . ((kb_layout . "us")
              (kb_variant . "")
              (kb_model . "")
              (kb_options . "ctrl:nocaps")
              (kb_rules . "evdev")
              (numlock_by_default . "true")
              (follow_mouse . "1")
              (repeat_delay . "250")
              (repeat_rate . "65")
              (force_no_accel . "false")
              (float_switch_override_focus . "2")
              (sensitivity . "0.2")
              (touchpad . ((natural_scroll . "true")
                           (scroll_factor . "0.4")
                           (disable_while_typing . "false")))))
    (general . ((gaps_in . "5")
                (gaps_out . "8")
                (border_size . "1")
                (col.active_border
                 ;; that long decimal is equal to arctan(9/16),
                 ;; i.e. angle of the hypotenuse of a 16:9 monitor
                 . ,(format #f "rgb(~a) rgb(~a) 29.357753542791272deg)"
                            (remove-hash (assoc-ref base16-colors 'base0D))
                            (remove-hash (assoc-ref base16-colors 'base0E))))
                (col.inactive_border
                 . ,(format #f "rgb(~a)"
                            (remove-hash (assoc-ref base16-colors 'base03))))
                (resize_on_border . "true")
                (layout . "dwindle")))
    (misc . ((disable_hyprland_logo . "true")
             (vrr . "1")
             (mouse_move_enables_dpms . "true")
             (key_press_enables_dpms . "true")
             (enable_swallow . "true")
             (swallow_regex . "[Kk][Ii][Tt][Tt][Yy]")))
    (master . ((new_on_top . "true")))
    (dwindle . ((force_split . "2")))
    
    ;; animations
    (animations . ((bezier . ("linear, 0, 0, 1, 1"
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
                              "md2, 0.4, 0, 0.2, 1"))
                   (animation . ("windows, 1, 3, md3_decel, popin 60%"
                                 "windowsIn, 1, 3, overshot, popin 60%"
                                 "windowsOut, 1, 3, md3_accel, popin"
                                 "border, 1, 3, default"
                                 "fade, 1, 3, md3_decel"
                                 "layersIn, 1, 3, menu_decel, slide top"
                                 "layersOut, 1, 3, menu_decel, slide top"
                                 "fadeLayersIn, 1, 3, menu_decel"
                                 "fadeLayersOut, 1, 3, menu_accel"
                                 "workspaces, 1, 3, hyprnostretch, slide"
                                 "specialWorkspace, 1, 3, md3_decel, slidevert"))
                   (enabled . "true")))
    (windowrulev2 . ("opacity 0.0 override 0.0 override,class:^(xwaylandvideobridge)$"
                     "noanim,class:^(xwaylandvideobridge)$"
                     "noinitialfocus,class:^(xwaylandvideobridge)$"
                     "maxsize 1 1,class:^(xwaylandvideobridge)$"
                     "noblur,class:^(xwaylandvideobridge)$"))
    (bind . ("SUPER,return,exec,kitty"
             "SUPER,E,exec,emacsclient -c -a emacs"
             "SUPERSHIFT,escape,exec,pkill Hyprland"

             ;; general window operations
             "SUPER,space,togglefloating,"
             "SUPER,G,togglegroup,"
             "SUPER,C,changegroupactive,"
             "SUPER,R,exec,bemenu-run --line-height 21"
             "SUPER,T,pseudo,"
             "SUPER,M,fullscreen,"
             "SUPER,Escape,exec,swaylock -f -e -l -L -s fill"
             "CTRLSUPER,Escape,exec,swaylock -f -e -l -L -s fill; sleep 1; loginctl suspend"
             "SUPERSHIFT,Q,killactive,"
             "SUPERSHIFT,T,exec,~/.config/hypr/scripts/switchLayout"

             "SUPER,H,movefocus,l"
             "SUPER,L,movefocus,r"
             "SUPER,K,movefocus,u"
             "SUPER,J,movefocus,d"
             "SUPERSHIFT,return,layoutmsg,swapwithmaster master"

             ;; focus workspaces
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

             ;; moving windows to workspaces
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
             ",XF86AudioNext,exec,mpc next"
             ",XF86AudioPrev,exec,mpc prev"
             "SUPER,down,exec,mpc toggle"
             "SUPER,up,exec,mpc toggle"
             ",XF86AudioPlay,exec,mpc toggle"
             "SUPER,right,exec,mpc next"
             "SUPER,left,exec,mpc prev"

             ;; screenshots
             "SUPER,S,exec,grimshot save area ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"
             "SUPERSHIFT,S,exec,grimshot save screen ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png"))

    ;; repeatable keybindings
    (binde . ("SUPERSHIFT,H,resizeactive,-20 0" ;; adjusting window size
              "SUPERSHIFT,L,resizeactive,20 0"
              "SUPERSHIFT,K,resizeactive,0 -20"
              "SUPERSHIFT,J,resizeactive,0 20"

              ;; media keys
              ",XF86AudioRaiseVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ +5%"
              ",XF86AudioLowerVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ -5%"

              ;; brightness control
              ",XF86MonBrightnessUp,exec,brightnessctl set +5%"
              ",XF86MonBrightnessDown,exec,brightnessctl set 5%-"))
    (bindm . ("SUPER, mouse:272, movewindow"
              "SUPER, mouse:273, resizewindow"))))

;; serialization currently only supports the following types:
;; - list of pairs
;; - list of strings
;; - string

(define (pair->hyprlang pair)
  (if (string? (cdr pair))
      ;; second argument being string should just be foo = bar
      (string-append (symbol->string (car pair)) " = " (cdr pair) "\n")
      ;; a list of strings means something like env, with multiple
      ;; options being set, e.g.
      ;; env = FOO,bar
      ;; env = BAR,1
      (if (and (list? (cdr pair)) (string? (car (cdr pair))))
          (apply string-append (map (lambda (val) (string-append (symbol->string (car pair)) " = " val "\n")) (cdr pair)))
          ;; a pair represents a nested section in hyprlang
          (if (and (list? (cdr pair)) (pair? (car (cdr pair))))
              (string-append (symbol->string (car pair)) " {\n"
                             (apply string-append (map pair->hyprlang (cdr pair)))
                             "}\n")
              (error "Unsupported type in hyprland config")))))

(define (hyprland-config->hyprlang config)
  (apply string-append (map pair->hyprlang config)))

(define (hyprland-config->file hyprland-conf)
  (run-with-store (open-connection)
                  (text-file* "hyprland.conf"
                              (hyprland-config->hyprlang hyprland-conf))))

(define (hyprland-configuration->files hyprland-conf)
  `((".config/hypr/hyprland.conf" ,(hyprland-config->file hyprland-conf))))

(define home-hyprland-service-type
  (service-type
   (name 'home-hyprland-service)
   (extensions
    (list (service-extension home-files-service-type
                             hyprland-configuration->files)
          (service-extension home-profile-service-type
                             (lambda (config)
                               ;;  hyprland needs to be installed at
                               ;; the system level for login managers
                               ;; to work
                               (list ;; (replace-mesa hyprland)
                                xdg-desktop-portal-hyprland
                                hyprpicker
                                hyprland-qtutils)))
          (service-extension home-activation-service-type
                             (lambda (config)
                               #~(invoke "hyprctl" "reload")))))
   (description "Create a hyprland configuration from a scheme object.")
   (default-value '())))
