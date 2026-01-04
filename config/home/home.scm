(define-module (config home home)
  #:use-module (config home services emacs)
  #:use-module (config home services fonts)
  #:use-module (config home services kitty)
  #:use-module (config home services hyprland)
  #:use-module (config home services noctalia-shell)
  #:use-module (config home services zsh)
  #:use-module (config packages fonts)
  #:use-module (config packages pokemon-colorscripts)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix import utils)
  #:use-module (nongnu packages nvidia))

(home-environment
 (packages (specifications->packages
             '("git"
               "librewolf"
               "blueman"
               "grayjay"
               "imagemagick"
               "mpv"
               "clonehero"
               "direnv"
               "ripgrep"

               ;; gpg
               "gnupg"
               "pinentry-qt"

               ;; desktop stuff
               "bemenu"
               "mako"
               "batsignal"
               "brightnessctl"
               "grimshot")))
 (services
  (append (list
           my/home-zsh-service
           my/home-font-service
           my/home-kitty-service
           my/home-emacs-service
           (service home-dotfiles-service-type
                    (home-dotfiles-configuration
                     (directories '("./files/"))))
           (service home-hyprland-service-type my/hyprland-config)
           (service home-noctalia-service-type
                    (noctalia-configuration
                     (colors my/noctalia-colors)))
           (service home-dbus-service-type)
           (service home-pipewire-service-type)
           (service home-gpg-agent-service-type
                    (home-gpg-agent-configuration (pinentry-program
                                                   (file-append
                                                    pinentry-qt
                                                    "/bin/pinentry-qt")))))
          %base-home-services)))
