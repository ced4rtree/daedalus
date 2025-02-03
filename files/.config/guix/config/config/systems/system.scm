(define-module (config systems system)
  #:use-module (gnu)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu packages linux))

(use-package-modules networking shells linux wm)
(use-service-modules cups desktop networking ssh xorg sddm dbus)

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
                    '("bluez" "bluez-alsa" "unzip"
                      "hypridle" "xdg-desktop-portal-hyprland"
                      "swaybg" "hyprpicker" "pinentry" "gnupg"))
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
                           (guix-service-type config =>
                                              (guix-configuration
                                               (inherit config)
                                               (substitute-urls
                                                (append '("https://substitutes.nonguix.org")
                                                        %default-substitute-urls))
                                               (authorized-keys
                                                (append (list (local-file "../../../signing-key.pub"))
                                                        %default-authorized-guix-keys)))))))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
                      (target (uuid
;;                               "e20a4940-90c0-4c5c-b2d4-83e6b0c6196a"
			       "a5ee9560-e067-45e8-931b-ce2013eef3bd")))))

 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "0141-9435"
;;                       (device (uuid "6EB3-2AC2"
                                     'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device (uuid
                                "2bbbd10e-d26e-4427-900e-35cd93ce62ee"
;;                                "a3e31e0e-0938-4e64-bd91-bf9a2c92717f"
                                'ext4))
                       (type "ext4")) %base-file-systems)))
