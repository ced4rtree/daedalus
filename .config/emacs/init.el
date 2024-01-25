(if (not (boundp 'has-restarted))
    (setq has-restarted nil)
  (setq has-restarted t))

(when (not has-restarted)
  (setq config-dir user-emacs-directory)) ;; to use for some stuff like autostart.sh for example, which I do want in my default user-emacs-directory
(setq user-emacs-directory "~/.local/share/emacs/")

(require 'package)
(setq package-user-dir (concat user-emacs-directory ".local/elpa"))
(setq package-gnupghome-dir (concat user-emacs-directory ".local/elpa/gnupg"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq use-package-always-ensure t)

(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-13"))

(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin t))

(global-hl-line-mode 1)

(use-package doom-modeline
  :ensure t
  :ensure octicons
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35
        doom-modeline-project-detection 'file-name
        doom-modeline-buffer-encoding nil
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-mu4e t))

(global-display-line-numbers-mode 1)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; 1 line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 101) ;; scroll one line at a time when moving the cursor down the page

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'java-mode-hook 'java-ts-mode)
(add-hook 'c-mode-hook 'c-ts-mode)
(add-hook 'c++-mode-hook 'c++-ts-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode 'rust-ts-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(global-visual-line-mode 1)

(add-to-list 'default-frame-alist '(alpha-background .  100))

(use-package org-tempo
  :ensure nil)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-leading-stars nil)

(use-package toc-org
  :ensure t
  :hook (org-mode . (lambda () (interactive) (toc-org-mode 1))))

(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . (lambda () (interactive) (org-auto-tangle-mode 1))))

(setq org-src-fontify-natively t ;; use the font like it is in a normal buffer
      org-src-tab-acts-natively t ;; tab works like it does in a normal buffer
      org-confirm-babel-evaluate nil ;; don't ask to evaluate code
      org-src-window-setup 'current-window) ;; have the org-edit-special command consume the current window

(setq org-agenda-files (list "~/org/agenda/schedule.org"))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook #'(lambda ()
                             (interactive)
                             (set-face-attribute 'org-level-1 nil :height 1.3)
                             (set-face-attribute 'org-level-2 nil :height 1.2)
                             (set-face-attribute 'org-level-3 nil :height 1.1)))

(use-package org-ref :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1)
  :after vertico)

(use-package prescient
  :ensure t
  :ensure vertico-prescient
  :after vertico
  :config
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1)
  :after vertico)

(use-package consult
  :ensure t
  :after vertico)

(use-package rust-mode :ensure t)
(use-package haskell-mode :ensure t)

(when (< emacs-major-version 29)
  (use-package eglot :ensure t))
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode #'eglot-ensure)
(add-hook 'haskell-mode #'eglot-ensure)
(setq eglot-autoshutdown t)
(use-package eglot-java
  :hook (java-ts-mode . eglot-ensure))

(use-package magit
  :defer t
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package projectile-ripgrep
  :ensure t
  :ensure-system-package rg
  :after projectile)

(use-package consult-projectile
  :ensure t
  :after projectile
  :after consult)

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package evil-nerd-commenter
  :ensure t
  :bind ("C-c C-/" . evilnc-comment-or-uncomment-lines))

(use-package corfu
  :ensure t
  :ensure nerd-icons-corfu
  :ensure nerd-icons
  :custom
  (corfu-auto t "Enable auto completion")
  :hook (prog-mode . corfu-mode))

(use-package recentf
  :ensure t
  :config
  ;; remove boilerplate files from recentf list
  (add-to-list 'recentf-exclude "~/org/agenda/schedule.org")
  (add-to-list 'recentf-exclude (concat user-emacs-directory "bookmarks")))

(use-package dashboard
  :ensure page-break-lines
  :ensure all-the-icons
  :after projectile
  :after recentf
  :hook (dashboard-mode . (lambda () (interactive) (page-break-lines-mode 1)))
  :hook (dashboard-mode . (lambda () (interactive) (display-line-numbers-mode -1)))
  :ensure t
  :init
  (setq dashboard-page-separator "

" ;; tell dashboard to use nice looking lines for section seperation

        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")) ;; tell emacs to use dashboard as startup screen
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5))
        dashboard-center-content t
        dashboard-startup-banner (concat config-dir "dash.txt")
        dashboard-icon-type 'all-the-icons
        dashboard-set-navigator t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-display-icons-p t)
  (advice-add #'dashboard-replace-displayable :override #'identity)
  :config
  (dashboard-setup-startup-hook))

(use-package no-littering
  :ensure t)

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))

(setq-default tab-width 4
              c-basic-offset 4
              c-ts-mode-indent-offset 4
              c-ts-mode-indent-style 'bsd
              c-default-style "bsd"
              indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'c-ts-mode-indent-offset 'tab-width)
(indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package perspective
  :ensure t
  :defer nil
  :bind (("C-c p k" . persp-kill)
         ("C-c p p" . persp-switch)
         ("C-c p i" . persp-ibuffer)
         ("C-c p b" . persp-switch-to-buffer*))
  :config
  (setq persp-initial-frame-name "Main")
  (persp-mode))

(use-package persp-projectile
  :ensure t
  :after perspective
  :after projectile
  :config
  (defvaralias 'projectile-switch-project #'projectile-persp-switch-project)
  (defvaralias 'project-switch-project #'projectile-persp-switch-project))

(use-package dired-open
  :ensure t
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "mpv --loop")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv")
                                ("mp3" . "foot -e mpv")))
  :bind (:map dired-mode-map
              ("f" . dired-open-file)))

(setq backup-directory-alist '((".*" . "~/.cache/emacs/auto-saves")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves" t)))

(add-to-list 'display-buffer-alist
  (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defvar goodbye-message-list (list "Don't leave me!"
                                   "B-baka! It's not like I liked you anyway..."
                                   "Thank you for participating in this Aperture Science computer-aided enrichment activity."
                                   "Emacs, Emacs never changes."
                                   "Wake up, Mr. Stallman. Wake up and smell the ashes."
                                   "I don't think you want to do that."
                                   (concat "I'm sorry " user-login-name ", I'm afraid I can't do that.")
                                   "In case I don't see ya, good afternoon, good evening, and good night!"
                                   "Here's looking at you, kid."
                                   "I do wish we could chat longer, but I'm having an old friend for dinner..."
                                   "Life moves pretty fast. If you don't stop and look around once and a while you might miss it."
                                   "So long... partner."
                                   "I'll be right here..."
                                   "I think this just might be my masterpiece."
                                   "Where we go from there is a choice I leave to you."
                                   "Daisy, Daisy, give me your answer do."
                                   "Leaving? Emacs? Are you well?")
  "A list of messages used as prompts for the user when quiting emacs")
(defun quit-emacs (&rest STUFF)
  (interactive)
  (y-or-n-p (concat (nth (random (length goodbye-message-list))
                         goodbye-message-list)
                    " Really quit emacs?")))
(global-set-key (kbd "C-x C-c") (lambda ()
                                  (interactive)
                                  (when (quit-emacs)
                                    (save-buffers-kill-terminal))))

(use-package drag-stuff
  :ensure t
  :init (drag-stuff-global-mode 1))

(use-package sudo-edit :ensure t)

(use-package yasnippet
  :ensure t
  :ensure yasnippet-snippets
  :defer t
  :init
  (add-hook 'prog-mode-hook #'(lambda () (interactive) (yas-minor-mode 1)))
  (setq yas-snippet-dirs (list
                          (concat user-emacs-directory ".local/elpa/yasnippet-snippets-20230815.820/snippets/")
                          (concat config-dir "snippets/"))))

(use-package vterm
  :defer t
  :ensure t
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after vterm
  :ensure t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package emms
  :ensure t
  ;; :after exwm ;; exwm autostart is where mpd gets started
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-music-directory (concat (getenv "HOME") "/Music"))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq mpc-host "localhost:6600")
  :bind (("C-c m m" . emms-smart-browse)
         ("C-c m n" . emms-next)
         ("C-c m p" . emms-previous)
         ("C-c m t" . emms-toggle)
         ("C-c m z" . emms-shuffle)
         ("C-c m f" . emms-seek-forward)
         ("C-c m b" . emms-seek-backward)
         ("C-c m c" . emms-player-mpd-connect)
         ("C-c m r" . emms-player-mpd-update-all-reset-cache)

         :map emms-playlist-mode-map
         ("Z" . emms-shuffle)))

(use-package calfw-org
  :ensure t
  :ensure calfw
  :after calfw
  :config
  ;; hotfix: incorrect time range display
  ;; source: https://github.com/zemaye/emacs-calfw/commit/3d17649c545423d919fd3bb9de2efe6dfff210fe
  (defun cfw:org-get-timerange (text)
    "Return a range object (begin end text).
If TEXT does not have a range, return nil."
    (let* ((dotime (cfw:org-tp text 'dotime)))
      (and (stringp dotime) (string-match org-ts-regexp dotime)
           (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
                  (start-date (nth 1 (car matches)))
                  (end-date (nth 1 (nth 1 matches)))
                  (extra (cfw:org-tp text 'extra)))
             (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
                 ( list( calendar-gregorian-from-absolute
                         (time-to-days
                          (org-read-date nil t start-date))
                         )
                   (calendar-gregorian-from-absolute
                    (time-to-days
                     (org-read-date nil t end-date))) text)))))))

(use-package mu4e
  :ensure-system-package mu
  :ensure-system-package mbsync

  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"

  :config
  (setq smtpmail-stream-type 'starttls ;; use tls for encryption
        mu4e-change-filenames-when-moving t ;; update file names as you move them around
        mu4e-update-interval (* 10 60) ;; update email every 10 minutes
        mu4e-hide-index-messages t ;; stop flashing my email to everyone around me
        mu4e-get-mail-command "mbsync -a" ;; requires isync to be installed and configured for your emails
        ;; NOTE: I recommend using .authinfo.gpg to store an encrypted set of your email usernames and passwords that mbsync pulls from
        ;; using the decryption function defined below
        message-send-mail-function 'smtpmail-send-it)

  ;; this is a dummy configuration for example
  ;; my real email info is stored in ~/.local/share/emacs/emails.el

  ;; mu4e-contexts (list
  ;;                (make-mu4e-context
  ;;                 :name "My email"
  ;;                 :match-func (lambda (msg)
  ;;                               (when msg
  ;;                                 (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
  ;;                 :vars '((user-mail-address . "myemail@gmail.com")
  ;;                         (user-full-name    . "My Name")
  ;;                         (smtpmail-smtp-server . "smtp.gmail.com")
  ;;                         (smtpmail-smtp-service . 587) ;; this is for tls, use 465 for ssl, 25 for plain
  ;;                         (mu4e-drafts-folder . "/[Gmail]/Drafts")
  ;;                         (mu4e-sent-folder . "/[Gmail]/Sent Mail")
  ;;                         (mu4e-refile-folder . "/[Gmail]/All Mail")
  ;;                         (mu4e-trash-folder . "/[Gmail]/Trash")))

  ;;                (make-mu4e-context
  ;;                 :name "My other email"
  ;;                 :math-func (lambda (msg)
  ;;                              (when msg
  ;;                                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
  ;;                 :vars '((user-mail-address . "koolkid37@example.com")
  ;;                         (user-full-name    . "koolkid")
  ;;                         (smtpmail-smtp-server . "smtp.example.com")
  ;;                         (smtpmail-smtp-service . 465) ;; this is for ssl, use 587 for ssl, 25 for plain
  ;;                         (mu4e-drafts-folder . "/Drafts")
  ;;                         (mu4e-sent-folder . "/Sent Mail")
  ;;                         (mu4e-refile-folder . "/All Mail")
  ;;                         (mu4e-trash-folder . "/Trash"))))

  (load (concat user-emacs-directory "emails.el")))

(use-package mu4e-alert
  :after mu4e
  :ensure t
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(defun efs/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

(defun bugger/emacs-reload ()
  (interactive)
  (setq has-restarted t)
  (org-babel-tangle-file (concat config-dir "config.org"))
  (load-file (concat config-dir "init.el"))
  (load-file (concat config-dir "init.el")))
(global-set-key (kbd "C-c C-r") 'bugger/emacs-reload)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c C-M-k") #'kill-all-buffers)

(setq gc-cons-threshold (* 2 1024 1024))
