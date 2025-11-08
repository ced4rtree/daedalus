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

;; don't restore session
(defmethod customize-instance ((browser browser) &key)
  (setf (slot-value browser 'restore-session-on-startup-p) nil))

(in-package #:nyxt-user)

(defvar *my-search-engines*
  (list
   (make-instance 'search-engine
                  :name "DuckDuckGo"
                  :shortcut "d"
                  #+nyxt-4 :control-url #+nyxt-3 :search-url
                  "https://duckduckgo.com/?q=~a")))

(define-configuration browser
    ((restore-session-on-startup-p nil)
     (external-editor-program "emacsclient -r")
     #+nyxt-4
     (search-engine-suggestions-p nil)
     #+nyxt-4
     (search-engines (append %slot-default% *my-search-engines*))))
