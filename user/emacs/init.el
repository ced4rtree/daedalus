;; -*- lexical-binding: t -*-

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(tab-bar-mode t)
(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))

(defun cedar/tab-name (tab)
  "Returns the name of TAB as a string."
  (cdr (assoc-string 'name tab)))

(defun cedar/open-name-in-tab (name always-perform-callback callback &rest callback-args)
  "Open/create a tab called NAME, and call CALLBACK upon opening.

If NAME is already a tab that exists, switch to it.  If there's not a
tab with the name NAME, then create a new tab with the name NAME and
call CALLBACK with the optionally supplied CALLBACK-ARGS.

If ALWAYS-PERFORM-CALLBACK is t, CALLBACK will always be performed with
its arguments, even if NAME is already an existing tab."

  (if (and (eq (length (tab-bar-tabs)) 1)
           (string-equal (cedar/tab-name (car (tab-bar-tabs))) "*scratch*"))
      (progn
        (tab-rename name)
        (apply callback callback-args))
    (let* ((tab-names (mapcar #'cedar/tab-name (tab-bar-tabs))))
      (if (and (member name tab-names) (not always-perform-callback))
          (tab-bar-switch-to-tab name)
        (progn
          (tab-bar-switch-to-tab name)
          (apply callback callback-args))))))

(use-package project
  :ensure nil
  :commands (project-prompt-project-dir)
  :config
  (defun cedar/project-switch-project-tab ()
    "Switch to a project tab, or create one if the prompted project doesn't exist."
    (interactive)
    (let* ((project-name (project-prompt-project-dir)))
      (cedar/open-name-in-tab project-name nil 'project-switch-project project-name)))

  (defun cedar/project-kill-buffers-and-tab ()
    "Kill all buffers in the current project and close the current tab."
    (interactive)
    (project-kill-buffers)
    ;; when the only tab open is a project, blindly closing it leaves
    ;; you on *scratch* but doesn't rename the buffer, which messes
    ;; with some tab opening settings
    (if (length> (tab-bar-tabs) 1)
        (tab-bar-close-tab)
      (when (string-equal (buffer-name) "*scratch*")
        (tab-bar-rename-tab "*scratch*"))))
  :bind (("C-x p p" . cedar/project-switch-project-tab)
         ("C-x p k" . cedar/project-kill-buffers-and-tab)))

(use-package elcord
  :custom
  (elcord-editor-icon "emacs_pen_icon")
  :commands elcord-mode
  :defines elcord-mode elcord-mode-icon-alist
  :config
  ;; https://github.com/Mstrodl/elcord/issues/17
  (defun elcord--enable-on-frame-created (f)
    (ignore f)
    (elcord-mode +1))

  (defun elcord--disable-elcord-if-no-frames (f)
    (when (let ((frames (delete f (visible-frame-list))))
            (or (null frames)
                (and (null (cdr frames))
                     (eq (car frames) terminal-frame))))
      (elcord-mode -1)
      (add-hook 'after-make-frame-functions 'elcord--enable-on-frame-created)))

  (defun my/elcord-mode-hook ()
    (if elcord-mode
        (add-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)
      (remove-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)))

  (add-hook 'elcord-mode-hook 'my/elcord-mode-hook)

  ;; elcord only has language icons setup for non-tree-sitter major modes, so I
  ;; have to add that manually
  (add-to-list 'elcord-mode-icon-alist '(java-ts-mode . "java-mode_icon"))
  (add-to-list 'elcord-mode-icon-alist '(c++-ts-mode . "cpp-mode_icon"))
  (add-to-list 'elcord-mode-icon-alist '(c-ts-mode . "c-mode_icon"))
  (add-to-list 'elcord-mode-icon-alist '(rust-ts-mode . "rust-mode_icon"))
  (add-to-list 'elcord-mode-icon-alist '(haskell-ts-mode . "haskell-mode_icon"))
  
  (elcord-mode))

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq backup-directory-alist '((".*" . "~/.cache/emacs/auto-saves")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves" t)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; (use-package hungry-delete
;;   :bind (("C-<backspace>" . hungry-delete-backward)
;;          ("C-M-d" . hungry-delete-forward)))

(defun cedar/scroll-page-and-point-up (&optional arg)
  "Scroll ARG lines up in a buffer, and maintain physical position of
the point.

The point does not change physical position on the screen, but does
scroll by ARG lines up to negate the buffer scrolling ARG lines down.

If LINES is not specified, 1 is assumed."

  (interactive)
  (let ((lines (if arg arg 1)))
    (scroll-down lines)
    (previous-line lines)))

(defun cedar/scroll-page-and-point-down (&optional arg)
  "Scroll ARG lines down in a buffer, and maintain physical position of
the point.

The point does not change physical position on the screen, but does
scroll by ARG lines down to negate the buffer scrolling ARG lines up.

If LINES is not specified, 1 is assumed."

  (interactive)
  (let ((lines (if arg arg 1)))
    (scroll-up lines)
    (next-line lines)))

;; (global-set-key (kbd "M-n") #'cedar/scroll-page-and-point-down)
;; (global-set-key (kbd "M-p") #'cedar/scroll-page-and-point-up)

(use-package direnv
  :config
  (direnv-mode t))

(use-package doom-themes
  :custom
  (doom-monokai-classic-brighter-comments t)
  :config
  (load-theme 'doom-monokai-classic t)
  (custom-set-faces
   '(default ((t (:background "#000000"))))
   '(tab-bar ((t (:foreground "#FFFFFF")))))
  (doom-themes-org-config))

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist '(alpha-background . 65))

(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-13"))

(setq visible-bell t
      ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "brown1")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg))))

(electric-pair-mode t)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(defalias #'yes-or-no-p #'y-or-n-p)

(use-package vertico
  :ensure marginalia
  :ensure vertico-prescient
  :ensure prescient
  :ensure orderless
  :ensure t

  :commands (vertico-mode
             marginalia-mode
             vertico-prescient-mode
             prescient-persist-mode
             vertico-directory-enter
             vertico-directory-delete-char
             vertico-directory-delete-word
             vertico-directory-tidy)
  :defines vertico-map

  :demand t
  :config
  (vertico-mode)
  (vertico-prescient-mode)
  (prescient-persist-mode)
  (marginalia-mode)

  (require 'vertico-directory)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  :custom
  (vertico-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :hook
  (prog-mode . corfu-mode)
  (corfu-mode . corfu-history-mode)
  (corfu-mode . corfu-echo-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-echo-delay 0))

(xterm-mouse-mode 1)

(which-key-mode t)

(setq-default tab-width 4
              c-basic-offset 4
              c-ts-mode-indent-offset 4
              c-ts-mode-indent-style 'bsd
              c-default-style "bsd"
              indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'c-ts-mode-indent-offset 'tab-width)
(indent-tabs-mode nil)
(defun cedar/change-tab-width (WIDTH)
  "Set the width of a tab to WIDTH in the current buffer."
  (setq-local tab-width WIDTH
              c-basic-offset WIDTH
              c-ts-mode-indent-offset WIDTH
              java-ts-mode-indent-offset WIDTH))

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'endless/colorize-compilation)

(use-package indent-bars
  :vc (:url "https://github.com/jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-starting-column 0)
  (indent-bars-color '(highlight :face-bg t :blend 0.7))
  :config
  (defun turn-off-indent-bars-mode ()
    "Turn off indent-bars-mode"
    (interactive)
    (indent-bars-mode -1))
  :hook (prog-mode . indent-bars-mode)
  :hook ((emacs-lisp-mode lisp-mode scheme-mode) . turn-off-indent-bars-mode))

(use-package ligature
  :commands (ligature-set-ligatures global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("--" "---" "==" "===" "!=" "!==" "=!="
                               "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                               "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                               "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                               "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                               "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                               "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                               "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                               "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                               "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                               "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                               ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                               "<:<" ";;;"))
  :hook
  (prog-mode . (lambda () (ligature-mode t)))
  (org-mode . (lambda () (ligature-mode t))))

(use-package rainbow-delimiters
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package beacon
  :config (beacon-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(defun cedar/remove-alligator-parens ()
  "Remove `<' and `>' as parens in org mode."

  (modify-syntax-entry ?< " ")
  (modify-syntax-entry ?> " "))
(add-hook 'org-mode-hook #'cedar/remove-alligator-parens)

;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; 1 line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      scroll-conservatively 101 ;; scroll one line at a time when moving the cursor down the page
      scroll-margin 8) ;; start scrolling 8 lines from the top/bottom

(setq-default frame-title-format "%b - Emacs")

(with-eval-after-load 'hideshow
  (add-hook 'prog-mode-hook #'hs-minor-mode))

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

(setq major-mode-remap-alist
      '((java-mode  . java-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (rust-mode . rust-ts-mode)))

(defun cedar/treesit-install-language-grammar ()
  (interactive)
  (let* ((lang (completing-read "Language: " '()))
         (path (concat " /tmp/tree-sitter-" lang)))
    (compile (concat "git clone https://github.com/tree-sitter/tree-sitter-" lang
                     path " --depth=1"
                     " && cd" path
                     " && echo \""
                     "     mkdir build"
                     "     && cd build"
                     "     && cmake ../"
                     "     && cmake --build ."
                     "     && if ! [ -d ~/.config/emacs/tree-sitter ]; then"
                     "            mkdir ~/.config/emacs/tree-sitter;"
                     "        fi"
                     "     && cp libtree-sitter-" lang ".so"
                     "        ~/.config/emacs/tree-sitter/\""
                     " >> build.sh"
                     " && guix shell gcc-toolchain make cmake bash -- bash build.sh"
                     " && exit"))))

(use-package magit :defer t)

(use-package haskell-mode)
(use-package stumpwm-mode)
(use-package cmake-mode)
(use-package markdown-mode)
(use-package rust-mode)
(use-package nix-mode)
(use-package qml-mode)

(use-package mu4e
  :ensure nil

  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (starttls-use-gnutls t)
  (mail-user-agent 'mu4e-user-agent)
  (smtpmail-stream-type 'starttls) ;; use tls for encryption
  (mu4e-change-filenames-when-moving t) ;; update file names as you move them around
  (mu4e-update-interval (* 10 60)) ;; update email every 10 minutes
  (mu4e-hide-index-messages t) ;; stop flashing my email to everyone around me
  (mu4e-get-mail-command "mbsync -a") ;; requires isync to be installed and configured for your emails

  :config
  (require 'smtpmail)
  (add-to-list 'mu4e-bookmarks
               '(:query "maildir:/inbox"
                 :name "Inbox"
                 :key ?i
                 :favorite t))
  (load (concat user-emacs-directory "emails.el")) ;; where all my private info is stored

(defun cedar/mu4e-in-tab ()
  (interactive)
  (cedar/open-name-in-tab "MU4E (Mail)" nil #'mu4e))
:bind (("C-c o e" . cedar/mu4e-in-tab)))

(defun efs/lookup-password (&rest keys)
  "Lookup a password from ~/.authinfo.gpg using KEYS to index the desired password.

e.g. (efs/lookup-password :host \"example.com\" :user \"user\"), which
will find the password for user@example.com"

  (let ((result (apply #'auth-source-search keys)))
    (when result
      (funcall (plist-get (car result) :secret)))))

(use-package emms
  :commands (emms-all emms-smart-browse)
  :defines emms-playlist-mode-map
  :custom
  (emms-seek-seconds 5)
  ;; (emms-player-mpd-music-directory (concat (getenv "HOME") "/Music"))
  ;; (emms-player-mpd-server-name "localhost")
  ;; (emms-player-mpd-server-port "6600")
  ;; (mpc-host "localhost:6600")
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))

  :config
  (require 'emms-setup)
  ;; (require 'emms-player-mpd)

  (emms-all)
  ;; (emms-player-mpd-connect)
  (emms-mode-line-disable)
  (emms-mpris-enable)

  (defun cedar/emms-smart-browse-in-tab ()
    (interactive)
    (cedar/open-name-in-tab "EMMS (Music)" nil #'emms-smart-browse))

  :bind (("C-c m t" . emms-pause) ;; t for toggle
         ("C-c m n" . emms-next)
         ("C-c m p" . emms-previous)
         ("C-c m m" . cedar/emms-smart-browse-in-tab)
         ("C-c o m" . cedar/emms-smart-browse-in-tab)
         :map emms-playlist-mode-map
         ("Z" . emms-shuffle)))

(use-package org-tempo :ensure nil)

(setopt org-agenda-files '("~/org/agenda/")
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-start-day "-2d"
        org-agenda-start-on-weekday nil
        org-agenda-span 7
        org-agenda-window-setup 'current-window)

(defun cedar/open-agenda-in-tab ()
  "Go to an org agenda tab, creating one if it doesn't exist."
  (interactive)
  (cedar/open-name-in-tab "Agenda" t #'org-agenda nil "n"))
(global-set-key (kbd "C-c o a") #'cedar/open-agenda-in-tab)

(require 'org-indent)
(add-hook 'org-mode-hook #'org-indent-mode)

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(setq org-src-fontify-natively t ;; use the font like it is in a normal buffer
      org-src-tab-acts-natively t ;; tab works like it does in a normal buffer
      org-confirm-babel-evaluate nil ;; don't ask to evaluate code
      org-src-window-setup 'current-window) ;; have the org-edit-special command consume the current window

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package visual-fill-column
  :custom visual-fill-column-width 90
  :config
  (defun org-enable-center-text ()
    "Enables centered text in org mode."
    (interactive)
    (visual-fill-column-mode t)
    (setq visual-fill-column-center-text t))

  (defun org-disable-center-text ()
    "Disables centered text in org mode."
    (interactive)
    (visual-fill-column-mode nil)
    (setq visual-fill-column-center-text nil))

  (defun org-toggle-center-text ()
    "Toggles centered text in org mode."
    (interactive)
    (setq visual-fill-column-center-text
          (not visual-fill-column-center-text))
    (visual-fill-column-mode visual-fill-column-center-text)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package evil-leader
  :init
  (setq evil-want-keybinding nil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  ;; files
  (evil-leader/set-key
   "." 'find-file
   "f s" 'save-buffer
   "f f" 'find-file

   ;; windows
   "w k" 'kill-buffer-and-window
   "w c" 'delete-window
   "w w" 'other-window
   "w 1" 'delete-other-windows
   "w 0" 'delete-window

   ;; dired
   "d d" 'dired
   "d j" 'dired-jump

   ;; tabs
   "t o" 'tab-next
   "t O" 'tab-previous
   "t n" 'tab-new
   "t k" 'tab-close
   "t RET" 'tab-switch

   ;; projects
   "p p" 'cedar/project-switch-project-tab
   "p k" 'cedar/project-kill-buffers-and-tab
   "p f" 'project-find-file
   "p g" 'project-find-regexp
   "p r" 'project-replace
   "p c" 'project-compile
   "p e" 'project-eshell

   ;; music
   "m t" 'emms-pause
   "m n" 'emms-next
   "m p" 'emms-previous

   ;; open programs
   "o a" 'cedar/open-agenda-in-tab
   "o e" 'cedar/mu4e-in-tab
   "o m" 'cedar/emms-smart-browse-in-tab

   ;; help
   "h f" 'describe-function
   "h v" 'describe-variable
   "h k" 'describe-key

   ;; git
   "g g" 'magit))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<escape>") 'keyboard-quit)
  (define-key evil-normal-state-map (kbd "M-x") 'execute-extended-command))

(keymap-global-set "M-x" 'execute-extended-command)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq gc-cons-threshold (* 2 1024 1024))
