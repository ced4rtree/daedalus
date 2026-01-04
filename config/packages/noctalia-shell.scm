(define-module (config packages noctalia-shell)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages qt)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:export (noctalia-shell))

(define nvidia-quickshell (replace-mesa quickshell))

(define noctalia-bin
  (run-with-store
   (open-connection)
   (text-file* "noctalia-shell"
               "#!" bash "/bin/bash \n"
               "get_script_dir() \n"
               "{ \n"
               "    local SOURCE_PATH=\"${BASH_SOURCE[0]}\" \n"
               "    local SYMLINK_DIR \n"
               "    local SCRIPT_DIR \n"
               "    # Resolve symlinks recursively \n"
               "    while [ -L \"$SOURCE_PATH\" ]; do \n"
               "        # Get symlink directory \n"
               "        SYMLINK_DIR=\"$( cd -P \"$( dirname \"$SOURCE_PATH\" )\" >/dev/null 2>&1 && pwd )\" \n"
               "        # Resolve symlink target (relative or absolute) \n"
               "        SOURCE_PATH=\"$(readlink \"$SOURCE_PATH\")\" \n"
               "        # Check if candidate path is relative or absolute \n"
               "        if [[ $SOURCE_PATH != /* ]]; then \n"
               "            # Candidate path is relative, resolve to full path \n"
               "            SOURCE_PATH=$SYMLINK_DIR/$SOURCE_PATH \n"
               "        fi \n"
               "    done \n"
               "    # Get final script directory path from fully resolved source path \n"
               "    SCRIPT_DIR=\"$(cd -P \"$( dirname \"$SOURCE_PATH\" )\" >/dev/null 2>&1 && pwd)\" \n"
               "    echo \"$SCRIPT_DIR\" \n"
               "} \n"
               nvidia-quickshell "/bin/qs -p $(get_script_dir)/../share/noctalia-shell/")))

(define noctalia-shell
  (let ((version "3.7.5"))
    (package
     (name "noctalia-shell")
     (version version)
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/noctalia-dev/noctalia-shell")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0lpbx4zlv4i7ygl66hiqmm7rvlr46w0f63qhblnc1yx75xhyq6jc"))))
     (build-system copy-build-system)
     (propagated-inputs
      ;; TODO: gpu-screen-recorder is not currently packaged for guix.
      ;; I looked into packaging it myself, but it requires things like
      ;; the cuda runtime, and I'm not tryna figure allat out
      (list nvidia-quickshell
            brightnessctl
            cava
            cliphist
            ddcutil
            matugen
            wlsunset
            wl-clipboard
            imagemagick
            qtwayland
            qtmultimedia
            wget))
     (arguments
      (list
       #:install-plan #~'(("./" "share/noctalia-shell/"))
       #:phases #~(modify-phases
                      %standard-phases
                      (add-after
                       'install 'create-bin
                       (lambda _
                         (mkdir (string-append #$output "/bin/"))
                         (copy-recursively #$noctalia-bin
                                           (string-append #$output "/bin/noctalia-shell"))
                         (invoke "chmod" "+x" (string-append #$output "/bin/noctalia-shell")))))))
     (home-page "https://docs.noctalia.dev/")
     (synopsis "A sleek and minimal desktop shell thoughtfully crafted for Wayland.")
     (description "A beautiful, minimal desktop shell for Wayland that actually gets out of your way. Built on Quickshell with a warm lavender aesthetic that you can easily customize to match your vibe.")
     (license expat))))
