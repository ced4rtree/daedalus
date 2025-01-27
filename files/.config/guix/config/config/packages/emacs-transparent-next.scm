;; see https://issues.guix.gnu.org/71627
(define-module (config packages emacs-transparent-next)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xorg))

(define-public emacs-transparent-next
  (package
   (inherit emacs-next)
   (name "emacs-transparent-next")
   (inputs (modify-inputs (package-inputs emacs-next)
                          (prepend
                           libxrender
                           libxt)))))
