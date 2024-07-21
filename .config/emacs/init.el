(if (not (boundp 'has-restarted))
    (setq has-restarted nil)
  (setq has-restarted t))

(when (not has-restarted)
  (setq config-dir user-emacs-directory)) ;; to use for some stuff like autostart.sh for example, which I do want in my default user-emacs-directory
(setq user-emacs-directory "~/.cache/emacs/")

(require 'package)
(setq package-user-dir (concat user-emacs-directory ".local/elpa"))
(setq package-gnupghome-dir (concat user-emacs-directory ".local/elpa/gnupg"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defvar bugger/use-evil nil "Whether or not to use evil mode and the additional SPC- bindings")

(when bugger/use-evil
  (use-package evil
    :ensure t
    :init
    (setq evil-want-keybinding nil)
    :custom
    (evil-undo-system 'undo-redo)
    :config
    (evil-mode t))
  (use-package evil-collection
    :ensure t
    :after evil
    :config
    (evil-collection-init)))

(use-package vertico
  :ensure t
  :custom
  (vertico-cyle t)
  :config
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy):config
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

(use-package vertico-posframe
  :config
  (vertico-posframe-mode t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono Nerd Font-14"))

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
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
  (global-ligature-mode t))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package highlight-indent-guides
  :defer t
  :hook (prog-mode . (lambda () (interactive) (highlight-indent-guides-mode 1)))
  :hook (highlight-indent-guides-mode . (lambda ()
                                          (interactive)
                                          (setq highlight-indent-guides-method 'character)
                                          (set-face-foreground 'highlight-indent-guides-character-face "gray31"))))

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

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'menu-bar--display-line-numbers-mode-relative)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; 1 line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 101) ;; scroll one line at a time when moving the cursor down the page
(pixel-scroll-precision-mode t)

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-mode))
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/tree-sitter/tree-sitter-cmake")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(haskell "https://github.com/tree-sitter/tree-sitter-haskell")
	(java "https://github.com/tree-sitter/tree-sitter-java")))

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

(setq-default tab-width 4
              c-basic-offset 4
              c-ts-mode-indent-offset 4
              c-ts-mode-indent-style 'bsd
              c-default-style "bsd"
              indent-tabs-mode t)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'c-ts-mode-indent-offset 'tab-width)
(indent-tabs-mode nil)
(defun bugger/change-tab-width (WIDTH)
  "Set the width of a tab to WIDTH in the current buffer"
  (setq-local tab-width WIDTH
              c-basic-offset WIDTH
              c-ts-mode-indent-offset WIDTH
              java-ts-mode-indent-offset WIDTH))
;; (add-hook 'java-ts-mode-hook #'(lambda () (interactive) (bugger/change-tab-width 2)))

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package recentf
  :ensure t
  :config
  ;; remove boilerplate files from recentf list
  (add-to-list 'recentf-exclude "~/org/agenda/schedule.org")
  (add-to-list 'recentf-exclude (concat user-emacs-directory "bookmarks")))

(use-package dashboard
  :ensure page-break-lines
  :ensure all-the-icons
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

(add-to-list 'default-frame-alist '(alpha-background .  90))

(use-package no-littering
  :ensure t)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package perspective
  :ensure t
  :defer nil
  :commands persp-project-switch
  :bind (("C-c p k" . persp-kill)
         ("C-c p p" . persp-project-switch)
         ("C-c p i" . persp-ibuffer)
         ("C-c p b" . persp-switch-to-buffer*)
         ("C-c p ." . persp-switch))
  :config
  (setq persp-initial-frame-name "Main")
  (persp-mode)
  (defun persp-project-switch ()
    "Switches to a new project and creates a new perspective for that project"
    (interactive)
    (let ((project-dir (project-prompt-project-dir)))
      (persp-switch (file-name-nondirectory
                     (directory-file-name
                      (file-name-directory project-dir))))
      (project-switch-project project-dir))))

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
  :init (drag-stuff-global-mode t)
  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down)))

(use-package yasnippet
  :ensure t
  :ensure yasnippet-snippets
  :defer t
  :hook (prog-mode . (lambda () (interactive) (yas-minor-mode 1)))
  :init
  (setq yas-snippet-dirs (list
                          (concat user-emacs-directory ".local/elpa/yasnippet-snippets-20230815.820/snippets/")
                          (concat config-dir "snippets/"))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(require 'zone)
(zone-when-idle 300)

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

(setq org-agenda-files '("~/org/agenda"))

(use-package org-modern
  :custom
  (org-modern-fold-stars '(("◉" . "◉")
                           ("◯" . "◯")
                           ("◍" . "◍")
                           ("◈" . "◈")
                           ("◇" . "◇")))
  :config
  (global-org-modern-mode t))

(add-hook 'org-mode-hook #'(lambda ()
                             (interactive)
                             (set-face-attribute 'org-level-1 nil :height 1.2)
                             (set-face-attribute 'org-level-2 nil :height 1.1)
                             (set-face-attribute 'org-level-3 nil :height 1.05)))

(use-package org-ref :ensure t)

(add-to-list 'org-latex-classes
             '("apa7"
               "\\documentclass{apa7}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("apa6"
               "\\documentclass{apa6}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(use-package org-present
  :ensure t)

(use-package visual-fill-column
  :ensure t
  :defer t
  :hook (org-present-mode . visual-fill-column-mode)
  :custom
  visual-fill-column-center-text t
  visual-fill-column-width 110)

(use-package rust-mode :ensure t)
(use-package haskell-mode :ensure t)
(use-package nix-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package markdown-mode :ensure t)

(when (< emacs-major-version 29)
  (use-package eglot
    :ensure t))
(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t))
;; (add-hook 'c-ts-mode-hook #'eglot-ensure)
;; (add-hook 'c++-ts-mode-hook #'eglot-ensure)
;; (add-hook 'rust-ts-mode #'eglot-ensure)
;; (add-hook 'haskell-mode #'eglot-ensure)
;;  (use-package eglot-java
;;    :hook (java-ts-mode . eglot-ensure))

(use-package eglot-java
  :defer t
  :hook (eglot-managed-mode . (lambda ()
                                (interactive)
                                (when (or (string= major-mode "java-mode")
                                          (string= major-mode "java-ts-mode"))
                                  (eglot-java-mode t)))))

(use-package magit
  :defer t
  :ensure t
  :hook (magit-post-commit . evil-insert))

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package evil-nerd-commenter
  :ensure t
  :bind ("C-c C-/" . evilnc-comment-or-uncomment-lines))

(use-package direnv
  :config
  (direnv-mode))

(use-package corfu
  :ensure t
  :ensure nerd-icons-corfu
  :ensure nerd-icons
  :custom
  (corfu-auto t)
  (corfu-auto-delay .18)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (tab-always-indent t)
  :hook (prog-mode . corfu-mode))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 30)
  (blamer-max-commit-message-length 50)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

(use-package eldoc-box
  :defer t
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package calfw
  :ensure t
  :ensure calfw-org)
(use-package calfw-org
  :ensure t
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
  :ensure-system-package isync

  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e"

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
  ;; my real email info is stored in ~/.config/emacs/emails.el

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

  (load (concat config-dir "emails.el")))

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

(when bugger/use-evil
  (use-package general
    :after evil
    :config
    (general-evil-setup)
    (general-define-key
     :states 'normal
     :prefix "SPC"
     "." #'find-file
     "g" #'magit)
    (general-define-key
     :states 'normal
     :prefix "SPC f"
     "s" #'save-buffer
     "f" #'find-file)
    (general-define-key
     :states 'normal
     :prefix "SPC p"
     "." #'persp-switch
     "p" #'persp-project-switch
     "f" #'project-find-file
     "c" #'project-compile)
    (general-define-key
     :states 'normal
     :prefix "SPC h"
     "k" #'describe-key
     "m" #'describe-map
     "f" #'describe-function
     "v" #'describe-variable)
    (general-define-key
     :states 'normal
     :prefix "SPC d"
     "d" #'dired
     "j" #'dired-jump)
    (general-define-key
     :states 'normal
     :prefix "SPC w"
     "w" #'other-window
     "v" #'split-window-right
     "n" #'split-window-below
     "c" #'delete-window
     "k" #'kill-buffer-and-window
     "C" #'delete-other-windows)
    (general-define-key
     :states 'normal
     :prefix "SPC b"
     "b" #'consult-buffer
     "i" #'persp-ibuffer
     "I" #'ibuffer
     "k" #'kill-buffer
     "r" #'revert-buffer
     "p" #'previous-bufer
     "n" #'next-buffer)
    (general-define-key
     :states 'normal
     :prefix "SPC o"
     "a" #'org-agenda
     "c" #'cfw:open-org-calendar)))

(global-set-key (kbd "C-M-n") #'(lambda ()
                                  (interactive)
                                  (forward-line 1)
                                  (scroll-up-line 1)))
(global-set-key (kbd "C-M-p") #'(lambda ()
                                  (interactive)
                                  (forward-line -1)
                                  (scroll-down-line 1)))

(setq gc-cons-threshold (* 2 1024 1024))
