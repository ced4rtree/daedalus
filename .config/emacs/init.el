(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'package)
(setq package-user-dir (concat user-emacs-directory ".local/elpa"))
(setq package-gnupghome-dir (concat user-emacs-directory ".local/elpa/gnupg"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'default-frame-alist
	     '(font . "FiraMono Nerd Font-14"))

(use-package solaire-mode
  :config (solaire-global-mode t))

(use-package catppuccin-theme
  :custom (catppuccin-flavor 'frappe)
  :config (load-theme 'catppuccin t))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(defun bugger/line-nums ()
  (display-line-numbers-mode 1)
  (menu-bar--display-line-numbers-mode-relative))

(add-hook 'prog-mode-hook #'bugger/line-nums)
(add-hook 'org-mode-hook #'bugger/line-nums)

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
  :config (which-key-mode 1))

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

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package vertico
      :custom
      (vertico-cyle t)
      :config
      (keymap-set vertico-map "RET" #'vertico-directory-enter)
      (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
      (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
      (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
      (vertico-mode 1))

(use-package marginalia
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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org-tempo
  :ensure nil)

(setq org-agenda-files "~/org/agenda/")

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
(use-package cmake-mode)
(use-package markdown-mode)

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
  (tab-always-indent t)
  :hook (eglot-managed-mode . corfu-mode))

(use-package calfw
  :ensure t
  :ensure calfw-org)
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
      :ensure-system-package mu
      :ensure-system-package mbsync

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
  (persp-mode)
  (defun persp-project-switch ()
    "Switches to a new project and creates a new perspective for that project"
    (interactive)
    (let ((project-dir (project-prompt-project-dir)))
      (persp-switch (file-name-nondirectory
                     (directory-file-name
                      (file-name-directory project-dir))))
      (project-switch-project project-dir))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(orderless consult vertico-prescient prescient which-key toc-org solaire-mode rust-mode rainbow-delimiters perspective org-modern org-auto-tangle nix-mode mu4e-alert markdown-mode magit haskell-mode evil-nerd-commenter eglot-java direnv corfu cmake-mode catppuccin-theme calfw-org calfw)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
