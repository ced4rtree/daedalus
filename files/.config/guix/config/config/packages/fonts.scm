(define-module (config packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (config packages python-xyz)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages fontutils)
  #:use-module (config packages python-xyz))

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

(define-public font-hasklig
  (package
   (name "font-hasklig")
   (version "1.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/i-tu/Hasklig")
           (commit (string-append "v" version))))
     (sha256
      (base32
       "0qbmzjdazmjny8fqhv7r7lmh0sr38wbi82lpz882346l5s65qxcb"))))
   (build-system font-build-system)
   (arguments
    (list #:phases #~(modify-phases
                      %standard-phases
                      (add-before 'install 'build-fonts
                                  (lambda _
                                    (begin
                                      (substitute* "makeInstances.sh"
                                                   (("makeInstancesUFO") "makeinstancesufo"))
                                      (invoke "bash" "./makeInstances.sh")
                                      (invoke "./build.sh")
                                      (invoke "bash" "-c" "mv target/TTF/*.ttf ."))
                                    #t)))))
   (native-inputs
    (list python-fonttools-python2 python-afdko fontforge python-2.7 coreutils bash))
   (home-page "https://github.com/i-tu/Hasklig")
   (synopsis "A code font with monospaced ligatures")
   (description "Programming languages are limited to relatively few
characters. As a result, combined character operators surfaced quite early, such
as the widely used arrow (->), comprised of a hyphen and greater sign. It looks
like an arrow if you know the analogy and squint a bit.

Composite glyphs are problematic in languages such as Haskell which utilize
these complicated operators (=> -< >>= etc.) extensively. The readability of
such complex code improves with pretty printing. Academic articles featuring
Haskell code often use lhs2tex to achieve an appealing rendering, but it is of
no use when programming.

Some Haskellers have resorted to Unicode symbols (⇒, ← etc.), which are valid in
the ghc. However they are one-character-wide and therefore eye-strainingly
small. Furthermore, when displayed as substitutes to the underlying
multi-character representation, as vim2hs does, the characters go out of
alignment.

Hasklig solves the problem the way typographers have always solved ill-fitting
characters which co-occur often: ligatures. The underlying code stays the same —
only the representation changes.

Not only can multi-character glyphs be rendered more vividly, other problematic
things in monospaced fonts, such as spacing can be corrected.")
   (license silofl1.1)))
