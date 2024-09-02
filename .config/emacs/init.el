;; -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish
  :ensure (:wait t)
  :demand t)

(setq vc-follow-symlinks nil)

(global-set-key (kbd "C-c e") #'eshell)

(add-to-list 'default-frame-alist
	     '(font . "JetBrainsMono Nerd Font-15"))

(use-package solaire-mode
  :config (solaire-global-mode t))

(use-package catppuccin-theme
  :custom (catppuccin-flavor 'frappe)
  :config (load-theme 'catppuccin t))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; 1 line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      scroll-conservatively 101) ;; scroll one line at a time when moving the cursor down the page
(pixel-scroll-precision-mode t)

(use-package which-key
  :config (which-key-mode 1)
  :diminish which-key-mode)

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

(use-package vertico
  :custom
  (vertico-cyle t)
  :config
  (require 'vertico-directory)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode 1))

(use-package marginalia
      :ensure t
      :config
      (marginalia-mode 1)
      :after vertico)

(use-package prescient
  :after vertico
  :ensure vertico-prescient
  :config
  (require 'vertico-prescient)
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package consult
  :ensure t
  :after vertico)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(global-hl-line-mode 1)

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
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

(setq split-width-threshold 140)

(use-package ligature
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

(use-package org-tempo
  :ensure nil)

(setq org-agenda-files '("~/org/agenda/")
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-timestamp-if-deadline-is-shown t
      org-agenda-start-day "-2d"
      org-agenda-start-on-weekday nil
      org-agenda-span 7
      org-agenda-window-setup 'current-window)

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(setq org-src-fontify-natively t ;; use the font like it is in a normal buffer
      org-src-tab-acts-natively t ;; tab works like it does in a normal buffer
      org-confirm-babel-evaluate nil ;; don't ask to evaluate code
      org-src-window-setup 'current-window) ;; have the org-edit-special command consume the current window

(use-package rust-mode)
(use-package haskell-mode)
(use-package nix-mode)
;; (use-package cmake-mode)
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

(when (< emacs-major-version 29)
  (use-package eglot))
(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t))

(use-package eglot-java
  :defer t
  :hook (eglot-managed-mode . (lambda ()
				(interactive)
				(when (or (string= major-mode "java-mode")
					  (string= major-mode "java-ts-mode"))
				  (eglot-java-mode t)))))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster" :protocol ssh)
  :after eglot
  :config (eglot-booster-mode))

(use-package magit
  :defer t)

(use-package evil-nerd-commenter
  :ensure t
  :bind ("C-c C-/" . evilnc-comment-or-uncomment-lines))

(use-package direnv
  :config
  (direnv-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay .18)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :hook ((eglot-managed-mode emacs-lisp-mode) . corfu-mode))

(setq major-mode-remap-alist
      '((java-mode  . java-ts-mode)
        (c-mode . c-ts-mode)
        (rust-mode . rust-ts-mode)))

(use-package calfw)
(use-package calfw-org
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

(use-package emms
  :custom
  (emms-seek-seconds 5)
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))
  :config
  (require 'emms-setup)
  (emms-all)
  ;; (setq emms-player-mpd-music-directory (concat (getenv "HOME") "/Music"))
  ;; (setq emms-player-mpd-server-name "localhost")
  ;; (setq emms-player-mpd-server-port "6600")
  ;; (setq mpc-host "localhost:6600")
  :bind (("C-c m m" . emms-smart-browse)
         ("C-c m n" . emms-next)
         ("C-c m p" . emms-prev)
         :map emms-playlist-mode-map
         ("Z" . emms-shuffle)))

(use-package perspective
  :defer nil
  :commands persp-project-switch
  :bind (("C-c p k" . persp-kill)
         ("C-c p p" . persp-project-switch)
         ("C-c p i" . persp-ibuffer)
         ("C-c p b" . persp-switch-to-buffer*)
         ("C-c p ." . persp-switch))
  :custom ((persp-initial-frame-name "Main")
           (persp-suppress-no-prefix-key-warning t))
  :config
  (persp-mode 1)
  (defun persp-project-switch ()
    "Switches to a new project and creates a new perspective for that project"
    (interactive)
    (let ((project-dir (project-prompt-project-dir)))
      (persp-switch (file-name-nondirectory
                     (directory-file-name
                      (file-name-directory project-dir))))
      (project-switch-project project-dir))))

(setq backup-directory-alist '((".*" . "~/.cache/emacs/auto-saves")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves" t)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install))

(defalias #'yes-or-no-p #'y-or-n-p)

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(with-eval-after-load 'hideshow
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(setq gc-cons-threshold (* 2 1024 1024))
