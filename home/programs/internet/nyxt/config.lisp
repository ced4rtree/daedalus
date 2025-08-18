(in-package #:nyxt-user)

;; emacs keybinds
(define-configuration buffer
  ((default-modes
    (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

;; emacs keybindings
(define-configuration web-buffer
  ((default-modes
    (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

;; reduce tracking
(define-configuration web-buffer
  ((default-modes
    (pushnew 'nyxt/mode/reduce-tracking:reduce-tracking-mode %slot-value%))))

;; dark theme
(define-configuration browser
  ((theme theme:+dark-theme+)))
(define-configuration (web-buffer)
  ((default-modes (pushnew 'nyxt/mode/style:dark-mode %slot-value%))))

;; don't restore session
(defmethod customize-instance ((browser browser) &key)
  (setf (slot-value browser 'restore-session-on-startup-p) nil))
