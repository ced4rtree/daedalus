(define-module (config home services noctalia-shell)
  #:use-module (config packages noctalia-shell)
  #:use-module (config util colors)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (json)
  #:export (home-noctalia-service-type
            noctalia-configuration
            my/noctalia-colors))


;; swap a color name for its actual hex value
(define (replace-color pair)
  `(,(car pair) . ,(assoc-ref base16-colors (string->symbol (cdr pair)))))

(define my/noctalia-colors
  (let ((colors '((mPrimary . "base0D")
                  (mOnPrimary . "base03")
                  (mSecondary . "base0E")
                  (mOnSecondary . "base03")
                  (mTertiary . "base0B")
                  (mOnTertiary . "base03")
                  (mError . "base08")
                  (mOnError . "base03")
                  (mSurface . "base00")
                  (mOnSurface . "base07")
                  (mSurfaceVariant . "base02")
                  (mOnSurfaceVariant . "base07")
                  (mOutline . "base04")
                  (mShadow . "base03")
                  (mHover . "base0E")
                  (mOnHover . "base03"))))
    (map replace-color colors)))

;; only supports colors for now
(define-record-type* <noctalia-configuration>
  noctalia-configuration make-noctalia-configuration
  noctalia-configuration?
  ;; colors is an alist mapping each color name to a color, such as above
  (colors noctalia-colors (default '())))

(define (color-config->file colors)
  (run-with-store (open-connection)
                  (text-file* "colors.json"
                              (scm->json-string colors))))

(define (noctalia-configuration->files config)
  `((".config/noctalia/colors.json" ,(color-config->file (noctalia-colors config)))))

(define home-noctalia-service-type
  (service-type
   (name 'home-noctalia-config)
   (extensions
    (list (service-extension home-files-service-type
                             noctalia-configuration->files)
          (service-extension home-profile-service-type
                             (lambda (config)
                               (list noctalia-shell)))))
   (description "Configuration for Noctalia Shell")))
