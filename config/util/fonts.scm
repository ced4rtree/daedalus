(define-module (config util fonts)
  #:use-module (config packages fonts)
  #:use-module (nongnu packages fonts)
  #:use-module (gnu packages fonts)
  #:use-module (guix records)
  #:export (font-config
            font-name
            font-package
            my/monospace-font
            my/serif-font
            my/sans-serif-font
            my/emoji-font
            my/fonts))

(define-record-type* <font>
  font make-font
  font?
  (package get-font-package)
  (name get-font-name))

(define (font-name font) (get-font-name font))
(define (font-package font) (get-font-package font))

(define my/monospace-font
  (font (package font-jetbrains-mono-nerd-font)
        (name "JetBrainsMono Nerd Font Mono")))

(define my/sans-serif-font
  (font (package font-ubuntu)
        (name "Ubuntu")))

(define my/serif-font my/sans-serif-font)

(define my/emoji-font
  (font (package font-google-noto-emoji)
        (name "Noto Color Emoji")))

(define my/fonts (list my/monospace-font
                       my/sans-serif-font
                       my/serif-font
                       my/emoji-font))
