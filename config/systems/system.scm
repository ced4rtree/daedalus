(define-module (config systems system)
  #:use-module (gnu)
  #:use-module (gnu packages wm)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu packages linux))

(use-package-modules networking shells linux wm)
(use-service-modules cups desktop networking ssh xorg sddm dbus sound)

(operating-system
 (locale "en_US.utf8")
 (timezone "America/Boise")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "muh-desktop")
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (kernel-arguments '("modprobe.blacklist=nouveau"
                     ;; enable the next line if using wayland
                     "nvidia_drm.modeset=1"))
 
 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "cedar")
                (comment "Cedar")
                (group "users")
                (home-directory "/home/cedar")
                (shell (file-append zsh "/bin/zsh"))
                (supplementary-groups '("wheel" "netdev" "audio" "video" "lp")))
               %base-user-accounts))

 ;; Packages installed system-wide.
 (packages (append (specifications->packages
                    '("bluez" "bluez-alsa" "unzip" "hypridle"))
                   (list (replace-mesa hyprland))
                   %base-packages))

 

 (services
  (append (list
           (service openssh-service-type)
           (service cups-service-type)
           (service nvidia-service-type)
           (service bluetooth-service-type
                    (bluetooth-configuration
                     (auto-enable? #t)
                     (just-works-repairing 'always)))
           ;; (multi-profile 'multiple)))
           (simple-service 'dbus-extras
                           dbus-root-service-type
                           (list blueman))
           (set-xorg-configuration
            (xorg-configuration
             (keyboard-layout keyboard-layout)
             (modules (cons nvda %default-xorg-modules))
             (drivers '("nvidia")))))
          ;;            sddm-service-type))

          ;; This is the default list of services we
          ;; are appending to.
          (modify-services %desktop-services
                           ;;                           (delete gdm-service-type)
                           (delete geoclue-service-type)
                           (delete pulseaudio-service-type)
                           (guix-service-type config =>
                                              (guix-configuration
                                               (inherit config)
                                               (substitute-urls
                                                (append '("https://substitutes.nonguix.org")
                                                        %default-substitute-urls))
                                               (authorized-keys
                                                (append (list (local-file "../../signing-key.pub"))
                                                        %default-authorized-guix-keys)))))))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
                      (target (uuid
			       "9c9cba4d-7321-4e44-89df-10ff73ab2323")))))

 (mapped-devices (list (mapped-device
                         (source (uuid
                                  "b88ec118-7505-4239-a394-c002a5489b8d"))
                         (target "luks")
                         (type luks-device-mapping))))

 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "1DE3-C8BA"
                                     'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device "/dev/mapper/luks")
                       (type "btrfs")
                       (dependencies mapped-devices)) %base-file-systems)))
