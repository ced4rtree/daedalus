;; -*- lexical-binding: t -*-

;; use-package stuff
(setq use-package-always-ensure t)

;; register melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; ui improvements
(load-theme 'modus-vivendi-tinted t)
(setopt mode-line-end-spaces nil)
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(menu-bar-mode -1)

;; flash modeline instead of the screen
(setq visible-bell t
      ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "brown1")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg))))

;; completion
(icomplete-mode t)
(icomplete-vertical-mode t)
(fido-vertical-mode t)

(keymap-set icomplete-fido-mode-map "TAB" 'icomplete-force-complete)

;;; Match completion substrings that may be out of order
(defun bugger/override-fido-completion-styles ()
  (setq-local completion-styles '(flex partial-completion emacs22 emacs21)))

(defun bugger/insert-dash ()
  "Inserts the dash character, also known as a hyphen or minus (-)."
  (interactive)
  (insert-char (char-from-name "HYPHEN_MINUS")))

(add-hook 'icomplete-minibuffer-setup-hook 'bugger/override-fido-completion-styles)
(define-key icomplete-minibuffer-map (kbd "SPC") #'bugger/insert-dash)


;; autocomplete
(electric-pair-mode t)

;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; fun squigglies
(add-hook 'prog-mode-hook #'flymake-mode-on)

;; just have y-or-n not yes-or-no
(defalias #'yes-or-no-p #'y-or-n-p)

;; hideshow mode
(with-eval-after-load 'hideshow
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; mu4e
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"

  :custom
  (smtpmail-stream-type 'starttls) ;; use tls for encryption
  (mu4e-change-filenames-when-moving t) ;; update file names as you move them around
  (mu4e-update-interval (* 10 60)) ;; update email every 10 minutes
  (mu4e-hide-index-messages t) ;; stop flashing my email to everyone around me
  (mu4e-get-mail-command "mbsync -a") ;; requires isync to be installed and configured for your emails

  :config
  (load (concat user-emacs-directory "emails.el")))

;; password decryption (for mbsync)
  (defun efs/lookup-password (&rest keys)
    (let ((result (apply #'auth-source-search keys)))
      (if result
          (funcall (plist-get (car result) :secret))
        nil)))

;; emms
(use-package emms
  :custom
  (emms-seek-seconds 5)
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))

  :config
;;; (setq emms-player-mpd-music-directory (concat (getenv "HOME") "/Music"))
;;; (setq emms-player-mpd-server-name "localhost")
;;; (setq emms-player-mpd-server-port "6600")
;;; (setq mpc-host "localhost:6600")
  (require 'emms-setup)
  (emms-all)

  :bind (("C-c m t" . emms-pause) ;; t for toggle
         ("C-c m n" . emms-next)
         ("C-c m p" . emms-previous)
         ("C-c m m" . emms-smart-browse)
         :map emms-playlist-mode-map
         ("Z" . emms-shuffle)))

;; move backup files
(setq backup-directory-alist '((".*" . "~/.cache/emacs/auto-saves")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves" t)))

;; let me use the mouse in emacs pwetty pwease
(xterm-mouse-mode 1)

;; which-key
(which-key-mode t)

;; scrolling
;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; 1 line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      scroll-conservatively 101 ;; scroll one line at a time when moving the cursor down the page
      scroll-margin 8) ;; start scrolling 8 lines from the top/bottom

;; programming langs
(use-package rust-mode)
(use-package haskell-mode)
(use-package nix-mode)
(use-package cmake-mode)
(use-package markdown-mode)
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; eglot
(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  :config
  :bind (:map prog-mode-map
              ("C-c c c" . (lambda ()
			     (interactive)
			     (eglot-ensure)))
              ("C-c c r" . eglot-rename)
              ("C-c c k" . eglot-shutdown)
              ("C-c c f" . eglot-code-action-quickfix)))

(use-package eglot-java
  :defer t
  :hook (eglot-managed-mode . (lambda ()
    				(interactive)
    				(when (or (string= major-mode "java-mode")
    					  (string= major-mode "java-ts-mode"))
    				  (eglot-java-mode t))))
  :hook (java-mode . eglot-java-mode))

;; tab bar mode
(tab-bar-mode t)

;; tree-sitter
(setq major-mode-remap-alist
      '((java-mode  . java-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (rust-mode . rust-ts-mode)))

;; configure gc-cons-threshold to be reasonable
(setq gc-cons-threshold (* 2 1024 1024))

;; tabbing
(setq-default tab-width 4
              c-basic-offset 4
              c-ts-mode-indent-offset 4
              c-ts-mode-indent-style 'bsd
              c-default-style "bsd"
              indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'c-ts-mode-indent-offset 'tab-width)
(indent-tabs-mode nil)
(defun bugger/change-tab-width (WIDTH)
  "Set the width of a tab to WIDTH in the current buffer"
  (setq-local tab-width WIDTH
              c-basic-offset WIDTH
              c-ts-mode-indent-offset WIDTH
              java-ts-mode-indent-offset WIDTH))

;; indent bars
(use-package indent-bars
  :vc (:url "https://github.com/jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-starting-column 0)
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :config
  (defun turn-off-indent-bars-mode ()
    "Turn off indent-bars-mode"
    (interactive)
    (indent-bars-mode -1))
  :hook (prog-mode . indent-bars-mode)
  :hook (emacs-lisp-mode . turn-off-indent-bars-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((indent-bars :url "https://github.com/jdtsmith/indent-bars"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
