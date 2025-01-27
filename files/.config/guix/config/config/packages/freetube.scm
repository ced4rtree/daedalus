(define-module (config packages freetube)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages video)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (nonguix build-system chromium-binary))

(define-public freetube
  (package
   (name "freetube")
   (version "0.22.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/FreeTubeApp/FreeTube/releases/download/v"
           version
           "-beta/freetube-"
           version
           "-linux-portable-x64.zip"))
     (sha256
      (base32
       "06pdiph9z7b6y4vh2kcr6q2brbmb0i23s2lhymqv746vzx9ii4fd"))))
   (build-system chromium-binary-build-system)
   (native-inputs `(("unzip" ,unzip)))
   (arguments (list
               #:wrapper-plan
               #~(map (lambda (file)
                        (string-append "../" file))
                      '("freetube"
                        "libEGL.so"
                        "libffmpeg.so"
                        "libGLESv2.so"
                        "libvk_swiftshader.so"
                        "libvulkan.so.1"))

               #:install-plan
               #~(let ((path-prefix "local/opt/freetube/"))
                   (map (lambda (file)
                          (list (string-append "../" file) path-prefix))
                        '("chrome_100_percent.pak"
                          "chrome_200_percent.pak"
                          "chrome_crashpad_handler"
                          "chrome-sandbox"
                          "freetube"
                          "icudtl.dat"
                          "libEGL.so"
                          "libffmpeg.so"
                          "libGLESv2.so"
                          "libvk_swiftshader.so"
                          "libvulkan.so.1"
                          "LICENSE.electron.txt"
                          "LICENSES.chromium.html"
                          "locales"
                          "resources"
                          "resources.pak"
                          "snapshot_blob.bin"
                          "v8_context_snapshot.bin"
                          "vk_swiftshader_icd.json")))

               #:phases
               #~(modify-phases
                  %standard-phases
                  (add-before
                   'install-wrapper 'wrap-where-patchelf-does-not-work
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                            (let* ((out (assoc-ref outputs "out"))
                                   (bin (string-append out "/local/opt/freetube/freetube"))
                                   (wrapper (string-append out "/bin/freetube")))
                              (mkdir-p (dirname wrapper))
                              (make-wrapper wrapper bin
                                            `("LD_LIBRARY_PATH" ":"
                                              prefix
                                              (,(string-join
                                                 `(,(string-append out "/local/opt/freetube"))
                                                 ":")))))
                            #t)))))
   (home-page "https://freetubeapp.io")
   (synopsis "The Private YouTube Client")
   (description "FreeTube is a YouTube client for Windows (10 and later), Mac (macOS
10.15 and later), and Linux built around using YouTube more
privately. You can enjoy your favorite content and creators without
your habits being tracked. All of your user data is stored locally and
never sent or published to the internet. FreeTube grabs data by
scraping the information it needs (with either local methods or by
optionally utilizing the Invidious API). With many features similar to
YouTube, FreeTube has become one of the best methods to watch YouTube
privately on desktop")
   (license agpl3)))
