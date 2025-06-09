(define-module (config packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages fontutils))

(define-public python-fonttools-python2
  (package
   (inherit python-fonttools-minimal)
   (name "python-fonttools-python2")
   (version "3.44.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "fonttools" version ".zip"))
            (sha256
             (base32
              "0v6399g755f2hn1ry62i5b6gdinf2fpx2966v3bxh6bjw1accb5p"))))))
