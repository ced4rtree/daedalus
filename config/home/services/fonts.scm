(define-module (config home services fonts)
  #:use-module (config util fonts)
  #:use-module (gnu home services)
  #:export (my/home-font-service))

;; just installs all the fonts defined in (config util fonts)
(define my/home-font-service
  (simple-service 'font-packages
                  home-profile-service-type
                  (map font-package my/fonts)))
