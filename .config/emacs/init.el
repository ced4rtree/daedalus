(require 'package)
(setq package-user-dir "~/.config/emacs/.local/elpa")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

										;(unless (package-installed-p 'use-package)
										;(package-refresh-contents)
										;(package-install 'use-package))
										;(setq use-package-always-ensure t)

(defun bugger/extern-package (AUTHOR PACKAGE)
  "Installs an emacs package from the github link https://github.com/AUTHOR/PACKAGE"

  ;; create the installation directory if it doesn't exist
  (when  (not (file-exists-p (concat user-emacs-directory ".local/extern-package")))
	(mkdir (concat user-emacs-directory ".local/extern-package")))

  ;; clone the project if it doesn't exist
  (when (not (file-exists-p (concat user-emacs-directory ".local/extern-package/" PACKAGE)))
	(shell-command (concat "git clone https://github.com/" AUTHOR "/" PACKAGE " " user-emacs-directory ".local/extern-package/" PACKAGE)))

  ;; load the package
  (add-to-list 'load-path (concat user-emacs-directory ".local/extern-package/" PACKAGE))
  (require (intern (symbol-value 'PACKAGE))))

(require 'bind-key)
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil magit
  :config
  (setq evil-collection-mode-list '(dashboard))
  (evil-collection-init))

(add-to-list 'default-frame-alist
             '(font . "AnonymicePro Nerd Font Mono-15"))
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package highlight-indent-guides
  :defer t
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(load-theme 'doom-one t)
;; (use-package xresources-theme
;;   :ensure t)
;; (add-hook 'server-after-make-frame-hook #'(lambda ()
;; 											(interactive)
;; 											(load-theme 'xresources t)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)

  :init
  (setq projectile-dynamic-mode-line t)

  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'filename
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type 0
        doom-modeline-height 35)
  (when (daemonp)
    (setq doom-modeline-icon t))
  :config
  
  (add-hook 'ef-themes-post-load-hook #'doom-modeline-refresh-bars))

(when (file-exists-p "/sys/class/power_supply/")
  (display-battery-mode 1))

(use-package centaur-tabs
  :hook (server-after-make-frame . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs))

;; When started in daemon mode, centaur tabs does not work at all, so here is a fix
 (if (not (daemonp))
    (centaur-tabs-mode)

  (defun centaur-tabs--daemon-mode (frame)
    (unless (and (featurep 'centaur-tabs) (centaur-tabs-mode-on-p))
      (run-at-time nil nil (lambda () (centaur-tabs-mode)))))
  (add-hook 'after-make-frame-functions #'centaur-tabs--daemon-mode))

(global-display-line-numbers-mode 1)
(with-eval-after-load "dashboard"
  (add-hook 'dashboard-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1))))

(global-hl-line-mode)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; 2 lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 101) ;; scroll one line at a time when moving the cursor down the page
(pixel-scroll-precision-mode 1) ;; smooth scrolling

(use-package all-the-icons
  :after exwm)

(use-package page-break-lines
  :config (global-page-break-lines-mode))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude "~/org/agenda/schedule.org")
  (add-to-list 'recentf-exclude "~/org/agenda/todo.org")
  (add-to-list 'recentf-exclude "~/org/agenda/emacs.org")
  (add-to-list 'recentf-exclude "~/org/agenda/homework.org")
  (add-to-list 'recentf-exclude "~/.config/emacs/bookmarks"))

(use-package dashboard
  :after all-the-icons
  :after page-break-lines
  :after projectile
  :after recentf
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :config
  (dashboard-setup-startup-hook))

(use-package rainbow-mode
  :hook (prog-mode . (lambda () (interactive) (rainbow-mode 1))))
(use-package rainbow-delimiters
  :hook (prog-mode . (lambda () (interactive) (rainbow-delimiters-mode 1))))
(use-package rainbow-identifiers
  :hook (prog-mode . (lambda () (interactive) (rainbow-identifiers-mode 1))))

;; (add-to-list 'default-frame-alist '(alpha-background . 85))

(add-hook 'java-mode-hook 'java-ts-mode)

(global-visual-line-mode 1)

(use-package org-tempo
  :ensure nil)

(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-leading-stars nil)

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-src-window-setup 'current-window
      org-src-preserve-indentation t)

(setq org-agenda-files (list "~/org/agenda/todo.org"
                             "~/org/agenda/homework.org"
                             "~/org/agenda/emacs.org"
                             "~/org/agenda/schedule.org"))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package ivy
  :defer 0.1
  :diminish
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package counsel
  :after ivy
  :init
  (counsel-mode)
  (setq ivy-initial-inputs-alist nil)) ; Disable the "^" in interactive counsel commands like M-x

(use-package ivy-rich
  :after ivy
  :defer t
  :custom
  (ivy-virtual-abbreviate 'full
						  ivy-rich-switch-buffer-align-virtual-buffer t
						  ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :defer t
  :bind (:map evil-normal-state-map
			  ("/" . swiper-isearch)
			  ("n" . evil-search-previous)
			  ("N" . evil-search-next)))

(setq indent-tabs-mode t)
(setq-default tab-width 4
              c-basic-offset 4
              c-default-style "stroustrup")
(defvaralias 'c-basic-offset 'tab-width)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'abort-minibuffers)

(use-package company
  :defer t
  :ensure t
  :config
  (global-company-mode))

(use-package lsp-mode
  :defer t
  :hook (prog-mode . #'lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-l"))

										; extensions
(use-package lsp-haskell
  :defer t
  :after lsp-mode)
(use-package lsp-treemacs
  :defer t
  :after lsp-mode)
(use-package lsp-java
  :defer t
  :after lsp-mode)
(use-package lsp-ui
  :defer t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-doc-mode))

(use-package flycheck
  :defer t
  :config
  (global-flycheck-mode))

(use-package treemacs :defer t)
(use-package treemacs-evil :after (treemacs evil))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-all-the-icons :after treemacs)

(use-package projectile
  :config
  (projectile-mode +1))
(use-package projectile-ripgrep :after projectile)
(use-package counsel-projectile :after (projectile counsel))

(use-package perspective
  :ensure t
  :config
  (setq persp-mode-prefix-key "C-x x"))
(persp-mode)

(use-package persp-projectile
  :ensure t
  :after perspective
  :after projectile)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t :after yasnippet)
(use-package java-snippets :ensure t :after yasnippet)

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist  '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist  '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist  '("\\.js$\\'" . web-mode)))
(use-package emmet-mode
  :ensure t
  :after web-mode
  :hook (web-mode . emmet-mode))

(use-package evil-nerd-commenter :ensure t)

(use-package dired-open
  :ensure t
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "nsxiv")
								("jpg" . "nsxiv")
								("png" . "nsxiv")
								("mkv" . "mpv")
								("mp4" . "mpv")
								("mp3" . "mpv"))))
(use-package peep-dired
  :after dired
  :ensure t
  :hook (peep-dired . evil-normalize-keymaps)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "l") 'dired-open-file))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(bugger/extern-package "SebastienWae" "app-launcher")

;; create a global keyboard shortcut with the following code
;; emacsclient -cF "((visibility . nil))" -e "(emacs-run-launcher)"
(defun emacs-run-launcher ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame 
      (make-frame '((name . "emacs-run-launcher")
					;; (minibuffer . only)
					(fullscreen . 0) ; no fullscreen
					(undecorated . t) ; remove title bar
					;; (auto-raise . t) ; focus on this frame
					;; (tool-bar-lines . 0)
					;; (menu-bar-lines . 0)
					(internal-border-width . 10)
					(width . 80)
					(height . 15)))
    (unwind-protect
		(funcall (lambda ()
				   (interactive)
				   (centaur-tabs-local-mode)
				   (app-launcher-run-app)
				   (centaur-tabs-local-mode)))
	  (delete-frame))))

(defun bugger/keybindings ()
  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map (kbd "C-q") 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          (,(kbd "s-r") . exwm-reset)

          ;; exit
          (,(kbd "s-C-q") . exwm-exit)

          ;; app launcher
          (,(kbd "s-p") . app-launcher-run-app)

          ;; emacs keys to move between windows
          (,(kbd "s-h") . windmove-left)
          (,(kbd "s-l") . windmove-right)
          (,(kbd "s-k") . windmove-up)
          (,(kbd "s-j") . windmove-down)

          ;; vim keys to swap windows
          (,(kbd "C-s-h") . windmove-swap-states-left)
          (,(kbd "C-s-l") . windmove-swap-states-right)
          (,(kbd "C-s-k") . windmove-swap-states-up)
          (,(kbd "C-s-j") . windmove-swap-states-down)

          ;; terminal
          (,(kbd "s-<return>") . vterm-other-window)

          ;; Launch applications via shell command
          (,(kbd "C-s-7") . (lambda (command)
                               (interactive (list (read-shell-command "$ ")))
                               (start-process-shell-command command nil command)))
          
          ;; music
          (,(kbd "<XF86AudioRaiseVolume>") . (lambda ()
                                               (interactive)
                                               (start-process-shell-command
                                                "volume-raise"
                                                nil
                                                (concat (getenv "HOME") "snd up"))))
          (,(kbd "<XF86AudioLowerVolume>") . (lambda ()
                                               (interactive)
                                               (start-process-shell-command
                                                "volume-lower"
                                                nil
                                                (concat (getenv "HOME") "snd down"))))
		  (,(kbd "C-c m l") . emms-next)
		  (,(kbd "C-c m h") . emms-previous)
		  (,(kbd "C-c m p") . emms-pause)
		  (,(kbd "C-c m r") . emms-player-mpd-update-all-reset-cache)

		  ;; vterm
		  (,(kbd "C-c v") . vterm)

		  ;; eshell
		  (,(kbd "C-c e") . eshell)

          ;; brightness
          (,(kbd "<XF86MonBrightnessUp>") . (lambda ()
                                              (interactive)
                                              (start-process-shell-command
                                               "volume-raise"
                                               nil
                                               (concat (getenv "HOME") "real-brightness up"))))
          (,(kbd "<XF86MonBrightnessDown>") . (lambda ()
                                                (interactive)
                                                (start-process-shell-command
                                                 "volume-lower"
                                                 nil
                                                 (concat (getenv "HOME") "real-brightness down"))))
          ;; layout stuff
          (,(kbd "s-m") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-f") . exwm-floating-toggle-floating)

          ;; Switch workspace
          (,(kbd "s-w") . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,(- i 1)))))
                    (number-sequence 1 9))
		  (,(kbd "s-0") . (lambda ()
							(interactive)
							(exwm-workspace-switch-create 9))))))

(defun bugger/gpg-fix ()
  (setenv "GPG_AGENT_INFO" nil)
  (setq auth-source-debug t)

  (setq epg-gpg-program "gpg2")
  (require 'epa-file)
  (epa-file-enable)
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start)

  (require 'org-crypt)
  (org-crypt-use-before-save-magic))

;; function for renaming windows
(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer exwm-class-name))

(defun bugger/exwm-settings ()
  (setq exwm-workspace-number 10) ;; setting workspaces

  ;; systray
  ;; using polybar, so this doesnt work
  ;; (use-package exwm-systemtray
  ;; 	:config
  ;; 	(exwm-systemtray-enable))

  ;; set window names
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer))

(defun bugger/autostart ()
  (call-process "/bin/sh" (concat user-emacs-directory "autostart.sh")))

(use-package exwm
  ;;:if (not (daemonp))
  :config
  (bugger/exwm-settings)
  (bugger/gpg-fix)
  (bugger/keybindings)

  (exwm-enable)

  (bugger/autostart))

(use-package elfeed :ensure t)
(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (elfeed-org))
(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

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
  :after exwm ;; exwm autostart is where mpd gets started
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
  (setq mpc-host "localhost:6600"))

(defun emms-scratchpad ()
  "Spawns an emms frame to use as a scratchpad in a window manager"
  (interactive)
  (with-selected-frame 
      (make-frame '((name . "music")
					(minibuffer . t)
					(fullscreen . 0) ; no fullscreen
					(undecorated . t) ; remove title bar
					;;(auto-raise . t) ; focus on this frame
					;;(tool-bar-lines . 0)
					;;(menu-bar-lines . 0)
					(internal-border-width . 10)))
    (unwind-protect
        (emms-smart-browse)
      (delete-frame))))

(use-package calfw)
(use-package calfw-org :after calfw)

(defun calfw-scratchpad ()
  "Spawns an emms frame to use as a scratchpad in a window manager"
  (interactive)
  (with-selected-frame 
      (make-frame '((name . "cal")
					(minibuffer . t)
					(fullscreen . 0) ; no fullscreen
					(undecorated . t) ; remove title bar
					(internal-border-width . 10)
					(width . 80)
					(height . 11)))                 ;;(auto-raise . t) ; focus on this frame
    ;;(tool-bar-lines . 0)
    ;;(menu-bar-lines . 0)
    (unwind-protect
        (cfw:open-org-calendar)
      (delete-frame))))

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :config
  (setq smtpmail-stream-type 'starttls
        mu4e-change-filenames-when-moving t
		mu4e-update-interval (* 10 60)
		mu4e-compose-format-flowed t
		mu4e-hide-index-messages t ;; stop flashing my email to everyone around me
		mu4e-get-mail-command "mbsync -a" ;; requires isync to be installed and configured for your emails
		;; NOTE: I recommend using .authinfo.gpg to store an encrypted set of your email usernames and passwords that mbsync pulls from
		;; using the decryption function defined below
		message-send-mail-function 'smtpmail-send-it)

  ;; this is a dummy configuration for example
  ;; my real email info is stored in ~/.config/emacs/emails.el

  ;; mu4e-contexts (list
  ;; 			   (make-mu4e-context
  ;; 				:name "School"
  ;; 				:match-func (lambda (msg)
  ;; 							  (when msg
  ;; 								(string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
  ;; 				:vars '((user-mail-address . "myemail@gmail.com")
  ;; 						(user-full-name    . "My Name")
  ;; 						(smtpmail-smtp-server . "smtp.gmail.com")
  ;; 						(smtpmail-smtp-service . 587) ;; this is for tls, use 465 for ssl, 25 for plain
  ;; 						(mu4e-drafts-folder . "/[Gmail]/Drafts")
  ;; 						(mu4e-sent-folder . "/[Gmail]/Sent Mail")
  ;; 						(mu4e-refile-folder . "/[Gmail]/All Mail")
  ;; 						(mu4e-trash-folder . "/[Gmail]/Trash"))))

  (load (concat user-emacs-directory "emails.el")))

(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(defun efs/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
	(if result
		(funcall (plist-get (car result) :secret))
	  nil)))

(use-package general
  :ensure t
  :init (general-evil-setup t))

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(setq evil-undo-system 'undo-redo)

;; tab over the region
(general-define-key
 :states 'visual
 "TAB" (lambda ()
         (interactive)
         (tab-to-tab-stop)))

;; comment/uncomment the region
(general-define-key
 :states '(normal visual)
 "C-/" '(evilnc-comment-or-uncomment-lines :which-key "Comment lines"))

;; toggle tolding
(general-define-key
 :states 'normal
 "TAB" 'evil-toggle-fold)

;; delete a tab, not 4 spaces
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;; Better directory navigation in ivy
(eval-after-load 'ivy #'(lambda ()
						  (define-key ivy-mode-map (kbd "DEL") 'ivy-backward-delete-char)))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "f"   '(:ignore t :which-key "files")
 "f s" '(save-buffer :which-key "Save file")
 "."   '(find-file   :which-key "open file"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "w"   '(:ignore t              :which-key "windows")
 "w w" '(evil-window-next       :which-key "next window")
 "w v" '(evil-window-vsplit     :which-key "create new vertical window")
 "w n" '(evil-window-new        :which-key "create new window")
 "w q" '(evil-window-delete     :which-key "delete current window")
 "w k" '(kill-buffer-and-window :which-key "delete current window and buffer"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "b"   '(:ignore t                 :which-key "buffer")
 "b b" '(buffer-menu               :which-key "buffer menu")
 "b i" '(ibuffer                   :which-key "ibuffer")
 "b c" '(kill-this-buffer          :which-key "kill buffer")
 "b k" '(kill-this-buffer          :which-key "kill buffer")
 "b p" '(previous-buffer           :which-key "previous buffer")
 "b n" '(next-buffer               :which-key "next buffer")
 "b h" '(centaur-tabs-backward-tab :which-key "previous tab")
 "b l" '(centaur-tabs-forward-tab  :which-key "previous tab")
 "b r" '(revert-buffer             :which-key "reload buffer"))
(define-key evil-normal-state-map (kbd "q") #'(lambda ()
                                                (interactive)
                                                (when (buffer-modified-p)
                                                  (when (y-or-n-p "Buffer modified. Save?")
                                                    (save-buffer)))
                                                (kill-this-buffer)))
(define-key evil-normal-state-map (kbd "Q") #'(lambda ()
                                                (interactive)
                                                (when (buffer-modified-p)
                                                  (when (y-or-n-p "Buffer modified. Save?")
                                                    (save-buffer)))
                                                (kill-buffer-and-window)))

(add-hook 'ibuffer-mode-hook #'(lambda ()
								 (interactive)
								 (keymap-local-set (kbd "l") 'ibuffer-visit-buffer)
								 (keymap-local-set (kbd "j") 'evil-next-visual-line)
								 (keymap-local-set (kbd "k") 'evil-previous-visual-line)))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "d" '(:ignore t :which-key "dired")
 "d d" '(dired :which-key "open dired")
 "d p" '(peep-dired :which-key "toggle peep-dired")
 "d j" '(dired-jump :which-key "open dired at current directory"))
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)) ; use dired-find-file if not using dired-open package

(with-eval-after-load "evil"
  (add-hook 'dashboard-mode-hook #'(lambda ()
									 (interactive)
									 (evil-local-set-key 'normal (kbd "r") 'dashboard-jump-to-recents)
									 (evil-local-set-key 'normal (kbd "p") 'dashboard-jump-to-projects)
									 (evil-local-set-key 'normal (kbd "a") 'dashboard-jump-to-agenda)
									 (evil-local-set-key 'normal (kbd "l") 'dashboard-return)
									 (evil-local-set-key 'normal (kbd "e") #'(lambda ()
                                                                               (interactive)
                                                                               (find-file "~/.config/emacs/config.org")))
									 (evil-local-set-key 'normal (kbd "x") #'(lambda ()
                                                                               (interactive)
                                                                               (find-file "~/.config/xmonad/xmonad.org"))))))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "o"     '(:ignore t :which-key "org")
 "o a"   '(:ignore t :which-key "org agenda")
 "o a c" '(cfw:open-org-calendar :which-key "open org calendar")
 "o C"   '(cfw:open-org-calendar :which-key "open org calendar")
 "o a a" '(org-agenda :which-key "open org agenda")
 "o a t" '(org-agenda-todo :which-key "open todo list"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "g"   '(:ignore t :which-key "magit")
 "g g" '(magit :which-key "open magit")
 "g s" '(magit-status :which-key "status")
 "g b" '(magit-branch :which-key "branch")
 "g c o" '(magit-checkout :which-key "checkout")
 "g c b" '(magit-branch-and-checkout :which-key "create and checkout a branch")
 "g c c" '(magit-commit :which-key "commit")
 "g p l" '(magit-pull :which-key "pull")
 "g p s" '(magit-push :which-key "push"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "h" '(:ignore t :which-key "help")
 "h r" '(:ignore t :which-key "reload")
 "h v" '(describe-variable :which-key "describe variable")
 "h t" '(counsel-load-theme :which-key "load theme")
 "h f" '(describe-function :which-key "describe function"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "t" '(:ignore t :which-key "toggle")
 "t v" '(vterm-toggle :which-key "toggle vterm")
 "t c" '(company-mode :which-key "toggle company")
 "t l" '(lsp-mode :which-key "toggle lsp")
 "t w" '(visual-line-mode :which-key "toggle visual line mode"))

(defun bugger/emacs-reload ()
  (interactive)
  (org-babel-tangle-file (concat user-emacs-directory "config.org"))
  ;; (byte-compile-file (concat user-emacs-directory "init.el"))
  (load-file "~/.config/emacs/init.el")
  (load-file "~/.config/emacs/init.el"))

(defun bugger/reload (mode)
  "Reload the mode specified by mode. mode must be a function"
  (funcall mode)
  (funcall mode))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "r" '(:ignore t :which-key "reload")
 "r r" '(bugger/emacs-reload :which-key "reload emacs")
 "r c" '(lambda () (interactive) (bugger/reload 'company-mode) :which-key "reload company")
 "r t" '(lambda () (interactive) (bugger/reload 'centaur-tabs-mode) :which-key "reload tabs")
 "r l" '(lambda () (interactive) (bugger/reload 'lsp-mode) :which-key "reload lsp"))

(which-key-add-key-based-replacements "SPC r c" "reload company")
(which-key-add-key-based-replacements "SPC r t" "reload tabs")
(which-key-add-key-based-replacements "SPC r l" "reload lsp")

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "/" '(counsel-projectile-rg :which-key "search project")
 "p" '(:ignore t :which-key "projectile")
 "p p" '(projectile-persp-switch-project :which-key "open project")
 "p c" '(projectile-compile-project :which-key "compile project")
 "p f" '(counsel-projectile-find-file-dwim :which-key "find file")
 "p a" '(projectile-add-known-project :which-key "add project"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "i" '(:ignore t :which-key "insert")
 "i s" '(yas-insert-snippet :which-key "snippets"))
(general-define-key
 :states 'insert
 :prefix "M-SPC"
 "i" '(:ignore t :which-key "insert")
 "i s" '(yas-insert-snippet :which-key "snippets"))

(general-define-key
 :prefix "SPC"
 :states '(normal visual)
 "m" '(:ignore t :which-key "music")
 "m m" '(emms :which-key "emms dashboard")
 "m n" '(emms-next :which-key "next song")
 "m p" '(emms-previous :which-key "prev song")
 "m r" '(emms-player-mpd-update-all-reset-cache :which-key "update database")
 "m b" '(emms-smart-browse :which-key "browse music")
 "m s" '(emms-shuffle :which-key "shuffle"))

(general-define-key
 :prefix "SPC"
 :states '(normal visual)
 "s" '(:ignore t :which-key "persp")
 "s b" '(persp-counsel-switch-buffer :which-key "switch buffer")
 "s i" '(persp-ibuffer :which-key "persp ibuffer")
 "s s" '(persp-switch :which-key "switch perspective")
 "s n" '(persp-next :which-key "next perspective")
 "s p" '(persp-prev :which-key "prev perspective")
 "s a" '(persp-add-buffer :which-key "add buffer to perspesctive")
 "s A" '(persp-set-buffer :which-key "brgin buffer to perspective")
 "s r" '(persp-remove :which-key "remove buffer from perspective")
 "s k" '(persp-kill :which-key "kill perspective")
 "s K" '(persp-kill-others :which-key "kill other perspectives"))

(setq gc-cons-threshold (* 2 1024 1024))
(server-start)
