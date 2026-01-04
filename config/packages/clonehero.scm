(define-module (config packages clonehero)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc))

(define-public clonehero
  (package
   (name "clonehero")
   (version "1.0.0.4080")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://github.com/clonehero-game/releases/releases/download/V"
       version
       "/CloneHero-linux.tar.xz"))
     (sha256
      (base32
       "14k5l3in1bfmdx3qmqa0s7mc3j1wfky8g5i48p51gx8h13xxaqk1"))))
   (build-system binary-build-system)
   (arguments
    (list
     #:install-plan #~'(("clonehero" "local/opt/clonehero/")
                        ("clonehero_Data" "local/opt/clonehero/")
                        ("UnityPlayer.so" "local/opt/clonehero/"))
     #:patchelf-plan #~'(("clonehero" ("gcc:lib" "glibc"))
                         ("UnityPlayer.so" ("gcc:lib" "glibc"))
                         ("clonehero_Data/MonoBleedingEdge/x86_64/libmonobdwgc-2.0.so"
                          ("gcc:lib" "glibc" "libz"))
                         ("clonehero_Data/MonoBleedingEdge/x86_64/libMonoPosixHelper.so"
                          ("libz"))
                         ("clonehero_Data/Plugins/libRtMidi.so" ("gcc:lib" "glibc" "alsa-lib"))
                         ("clonehero_Data/Plugins/libNativeFilePicker.so" ("gtk+:out" "glib:out"))
                         ("clonehero_Data/Plugins/libbass_fx.so" ("gcc:lib" "glibc"))
                         ("clonehero_Data/Plugins/libslimage.so" ("gcc:lib" "glibc")))
     #:phases #~(modify-phases %standard-phases
                               (add-before 'validate-runpath 'fix-unity-rpath
                                           (lambda _
                                             (invoke "patchelf" "--add-rpath"
                                                     (string-append #$output "/local/opt/clonehero/")
                                                     (string-append #$output "/local/opt/clonehero/clonehero"))))
                               (add-before 'patchelf 'patchelf-writable
                                           (lambda _
                                             (for-each make-file-writable
                                                       '("clonehero"))))
                               (add-after 'install 'make-bin-executable
                                          (lambda _
                                            (invoke "chmod" "+x"
                                                    (string-append
                                                     #$output
                                                     "/local/opt/clonehero/clonehero"))))
                               (add-after 'make-bin-executable 'symlink-bin-files
                                          (lambda _
                                            (mkdir (string-append #$output "/bin"))
                                            (symlink
                                             (string-append #$output "/local/opt/clonehero/clonehero")
                                             (string-append #$output "/bin/clonehero"))
                                            #t)))))
   (native-inputs (list patchelf coreutils))
   (inputs
    `(("gcc:lib" ,gcc "lib")
      ("glibc" ,glibc)
      ("libz" ,zlib)
      ("alsa-lib" ,alsa-lib)
      ("gtk+:out" ,gtk+ "out")
      ("glib:out" ,glib "out")))
   (home-page "https://clonehero.net/")
   (synopsis "Clone Hero is a classic instrument based rhythm game for Windows, Mac, Linux, and Android.")
   (description "Clone Hero is a classic instrument based rhythm game for Windows, Mac, Linux,
and Android. It's playable with any 5 or 6 fret guitar controller, any midi drum
kit, any game controller and even your keyboard! Jam out with Drums, 5-fret
Guitar, or 6-fret Guitar online or local!")
   (license (nonfree "https://clonehero.net/eula/"))))
