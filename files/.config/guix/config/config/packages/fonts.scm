(define-module (config packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module (guix licenses))

(define-public font-iosevka-nerd-font
  (package
   (name "font-iosevka-nerd-font")
   (version "3.3.0")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
       version
       "/Iosevka.zip"))
     (sha256
      (base32
       "10w24pir4flr0zhm0n6v6kblgmcx7dpnqv2xkp8d0rgh3rnlwpm5"))))
   (build-system font-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-before 'install 'make-files-writable
                                 (lambda _
                                   (for-each
                                    make-file-writable
                                    (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
                                   #t)))))
   (home-page "https://www.nerdfonts.com/")
   (synopsis "Nerd fonts variant of Iosevka font")
   (description
    "Nerd fonts variant of Iosevka font.  Nerd Fonts is a project that patches
developer targeted fonts with a high number of glyphs (icons).  Specifically to
add a high number of extra glyphs from popular 'iconic fonts' such as Font
Awesome, Devicons, Octicons, and others.")
   (license silofl1.1)))

(define-public font-jetbrains-mono-nerd-font
  (package
   (name "font-jetbrains-mono-nerd-font")
   (version "3.3.0")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
       version
       "/JetBrainsMono.zip"))
     (sha256
      (base32
       "1r6v5naj0g6wkhpr53zc7rygg9s199h81s7wf3x4nq0b6lm7i0rd"))))
   (build-system font-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-before 'install 'make-files-writable
                                 (lambda _
                                   (for-each
                                    make-file-writable
                                    (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
                                   #t)))))
   (home-page "https://www.nerdfonts.com/")
   (synopsis "Nerd fonts variant of JetBrains Mono font")
   (description
    "Nerd fonts variant of JetBrains Mono font.  Nerd Fonts is a project that patches
developer targeted fonts with a high number of glyphs (icons).  Specifically to
add a high number of extra glyphs from popular 'iconic fonts' such as Font
Awesome, Devicons, Octicons, and others.")
   (license silofl1.1)))

(define-public font-source-code-pro-nerd-font
  (package
   (name "font-source-code-pro-nerd-font")
   (version "3.3.0")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
       version
       "/SourceCodePro.zip"))
     (sha256
      (base32
       "1ihaz75xfka66cwzb086dw5wrk6skjm2sxnbszvmgiabl2yx3gji"))))
   (build-system font-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-before 'install 'make-files-writable
                                 (lambda _
                                   (for-each
                                    make-file-writable
                                    (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
                                   #t)))))
   (home-page "https://www.nerdfonts.com/")
   (synopsis "Nerd fonts variant of Source Code Pro")
   (description
    "Nerd fonts variant of Source Code Pro.  Nerd Fonts is a project that patches
developer targeted fonts with a high number of glyphs (icons).  Specifically to
add a high number of extra glyphs from popular 'iconic fonts' such as Font
Awesome, Devicons, Octicons, and others.")
   (license silofl1.1)))
