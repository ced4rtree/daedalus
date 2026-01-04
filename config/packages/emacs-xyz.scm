(define-module (config packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (config util colors)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:export (emacs-eglot-java
            emacs-elcord
            emacs-nerd-icons-dired
            emacs-stylix-theme))

(define emacs-eglot-java
  (let ((commit "b42b5190f3f59976d330fcec5fd27fc8e2701336")
        (revision "0")
        (url "https://github.com/yveszoundi/eglot-java"))
    (package
     (name "emacs-eglot-java")
     (version (git-version "0.1" revision commit))
     (source
      (origin
       (uri (git-reference
             (url url)
             (commit commit)))
       (method git-fetch)
       (sha256
        (base32 "0224qm3fhw7avl8npsrm32iwzbnwv4kihiyrkh5ps459mpawbjg3"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (home-page url)
     (synopsis "Java extension for the eglot LSP client ")
     (description "This package provides additional Java programming language support for eglot.")
     (license license:gpl3))))

(define emacs-elcord
  (let ((commit "deeb22f84378b382f09e78f1718bc4c39a3582b8")
        (revision "0")
        (url "https://github.com/Mstrodl/elcord"))
    (package
     (name "emacs-elcord")
     (version (git-version "0.1" revision commit))
     (source
      (origin
       (uri (git-reference
             (url url)
             (commit commit)))
       (method git-fetch)
       (sha256
        (base32 "1w9258vdl994l786vmhzx2xm8mb9rvc9v0fl12qib5fvfgk51d15"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (home-page url)
     (synopsis "Discord Rich Presence / Gamebridge support for Emacs!")
     (description "This package will connect with a local Discord client to update your status via the Discord Rich Presence API.")
     (license license:expat))))

(define emacs-nerd-icons-dired
  (let ((commit "3265d6c4b552eae457d50d423adb10494113d70b")
        (revision "0")
        (url "https://github.com/rainstormstudio/nerd-icons-dired"))
    (package
     (name "emacs-nerd-icons-dired")
     (version (git-version "0.1" revision commit))
     (source
      (origin
       (uri (git-reference
             (url url)
             (commit commit)))
       (method git-fetch)
       (sha256
        (base32 "1kkpw59xflz4i0jdg5rdw84lggjqjy2k03yilpa19a5allvar63s"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (propagated-inputs
      `(("emacs-nerd-icons" ,emacs-nerd-icons)))
     (home-page url)
     (synopsis "Use nerd-icons for Dired")
     (description "nerd-icons-dired is inspired by all-the-icons-dired")
     (license license:gpl3))))

;; this code is adapted from stylix, a theming manager for nix
;; https://github.com/nix-community/stylix/blob/551df12ee3ebac52c5712058bd97fd9faa4c3430/modules/emacs/hm.nix
(define stylix-theme-text
  (format #f "
(require 'base16-theme)

(defvar base16-stylix-theme-colors
  '(:base00 \"~a\"
   :base01 \"~a\"
   :base02 \"~a\"
   :base03 \"~a\"
   :base04 \"~a\"
   :base05 \"~a\"
   :base06 \"~a\"
   :base07 \"~a\"
   :base08 \"~a\"
   :base09 \"~a\"
   :base0A \"~a\"
   :base0B \"~a\"
   :base0C \"~a\"
   :base0D \"~a\"
   :base0E \"~a\"
   :base0F \"~a\")
  \"All colors for Base16 stylix are defined here.\")

;; Define the theme
(deftheme base16-stylix)

;; Add all the faces to the theme
(base16-theme-define 'base16-stylix base16-stylix-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-stylix)

;; Add path to theme to theme-path
(add-to-list 'custom-theme-load-path
    (file-name-directory
        (file-truename load-file-name)))

(provide 'base16-stylix-theme)"
          (assoc-ref base16-colors 'base00)
          (assoc-ref base16-colors 'base01)
          (assoc-ref base16-colors 'base02)
          (assoc-ref base16-colors 'base03)
          (assoc-ref base16-colors 'base04)
          (assoc-ref base16-colors 'base05)
          (assoc-ref base16-colors 'base06)
          (assoc-ref base16-colors 'base07)
          (assoc-ref base16-colors 'base08)
          (assoc-ref base16-colors 'base09)
          (assoc-ref base16-colors 'base0A)
          (assoc-ref base16-colors 'base0B)
          (assoc-ref base16-colors 'base0C)
          (assoc-ref base16-colors 'base0D)
          (assoc-ref base16-colors 'base0E)
          (assoc-ref base16-colors 'base0F)))

(define emacs-stylix-theme
  (package
   (name "emacs-stylix-theme")
   (version "1.0")
   (source (run-with-store
            (open-connection)
            (text-file* "base16-stylix-theme.el" stylix-theme-text)))
   (build-system emacs-build-system)
   (propagated-inputs `(("emacs-base16-theme" ,emacs-base16-theme)))
   (home-page "https://github.com/nix-community/stylix/blob/551df12ee3ebac52c5712058bd97fd9faa4c3430/modules/emacs/hm.nix")
   (synopsis "Base16 theme for emacs that conforms to the colors in (config util colors)")
   (description "Base16 theme for emacs that conforms to the colors in (config util colors)")
   (license license:expat)))
