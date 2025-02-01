(define-module (config home home)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (config packages fonts)
  #:use-module (config packages pokemon-colorscripts)
  #:use-module (config packages starship)
  #:use-module (config packages emacs-transparent-next)
  #:use-module (config packages freetube)
  #:use-module (nongnu packages nvidia)
  #:use-module (guix gexp))

(home-environment
 (packages (append
            (specifications->packages
             '("git"
               "emacs-next-pgtk"
               "mu"
               "isync"
               "firefox"
               "blueman"
               "freetube"
               "python"
               "python-wrapper"
               "imagemagick"
               "mpv"
               "clonehero"
               "direnv"
               "pinentry"

               ;; desktop stuff
               "waybar"
               "bemenu"
               "mako"
               "batsignal"
               "brightnessctl"

               ;; terminal
               "kitty"

               ;; fonts
               "font-iosevka-nerd-font"
               "font-jetbrains-mono-nerd-font"
               "font-terminus"
               "font-source-code-pro-nerd-font"
               "font-hasklig"

               ;; zsh stuff
               "pokemon-colorscripts"
               "starship"
               "zsh-syntax-highlighting"))))
 (services
  (list
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshrc (list (local-file "../../../../zsh/zshrc")))))
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories '("../../../../../"))))
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry "/bin/pinentry"))
             (extra-content "allow-loopback-pinentry")
             (ssh-support? #t))))))
