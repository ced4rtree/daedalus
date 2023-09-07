(when (not (boundp 'has-restarted))
  (setq has-restarted nil))

(setq mouse-autoselect-window t
      focus-follows-mouse t)

(when (not has-restarted)
  (setq config-dir user-emacs-directory)) ;; to use for some stuff like autostart.sh for example, which I do want in my default user-emacs-directory
(setq user-emacs-directory "~/.cache/emacs/")

(load (concat config-dir "packages.el"))

(require 'package)
(setq package-user-dir (concat user-emacs-directory ".local/elpa"))
(setq package-gnupghome-dir (concat user-emacs-directory ".local/elpa/gnupg"))
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
  ;; 
  ;; load the package
  (add-to-list 'load-path (concat user-emacs-directory ".local/extern-package/" PACKAGE))
  (require (intern (symbol-value 'PACKAGE))))

(require 'bind-key)
(when packages/evil
  (use-package evil
    :ensure t
    :init
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1)
    (evil-set-undo-system 'undo-redo)))

(when packages/evil
  (use-package evil-collection
    :ensure t
    :after evil magit
    :config
    (evil-collection-init)))

(add-to-list 'default-frame-alist
             '(font . "Iosevka Nerd Font Mono-15"))

(use-package highlight-indent-guides
  :defer t
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
;; (use-package xresources-theme
;;   :ensure t)
;; (add-hook 'server-after-make-frame-hook #'(lambda ()
;;                                             (interactive)
;;                                             (load-theme 'xresources t)))

(use-package all-the-icons
  :ensure t
  :after exwm) ;; this line needs to be fixed for when exwm is disabled

(when packages/doom-modeline
  (use-package doom-modeline
    :after all-the-icons
    :ensure t
    :hook (after-init . doom-modeline-mode)
    :hook (doom-modeline-mode . size-indication-mode)
    :hook (doom-modeline-mode . column-number-mode)

    :config
    (setq projectile-dynamic-mode-line t)

    ;; Set these early so they don't trigger variable watchers
    (setq doom-modeline-bar-width 3
          doom-modeline-github nil
          doom-modeline-mu4e t
          doom-modeline-persp-name t
          doom-modeline-minor-modes nil
          doom-modeline-major-mode-icon t
          doom-modeline-buffer-file-name-style 'filename
          ;; Only show file encoding if it's non-UTF-8 and different line endings
          ;; than the current OSes preference
          doom-modeline-buffer-encoding 'nondefault
          doom-modeline-default-eol-type 0
          doom-modeline-height 35
          doom-modeline-icon t)
    
    (when (package-installed-p 'ef-themes)
      (add-hook 'ef-themes-post-load-hook #'doom-modeline-refresh-bars))))

(when (or (file-exists-p "/sys/class/power_supply/BAT0") (file-exists-p "/sys/class/power_supply/BAT1"))
  (display-battery-mode 1))

(when packages/tabs
  (use-package centaur-tabs
    :ensure t
    :hook (prog-mode . (lambda ()
                         (interactive)
                         (centaur-tabs-mode 1)
                         (centaur-tabs-local-mode 1)))
    :hook (org-mode . (lambda ()
                        (interactive)
                        (centaur-tabs-local-mode -1)))
    :hook (fundamental-mode . (lambda ()
                                (interactive)
                                (centaur-tabs-local-mode -1)))
    :hook (dashboard-mode . (lambda ()
                              (interactive)
                              (centaur-tabs-local-mode -1)))
    :hook (text-mode . (lambda ()
                         (interactive)
                         (centaur-tabs-local-mode -1)))
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
          centaur-tabs-cycle-scope 'tabs)))

(global-display-line-numbers-mode 1)

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

(when packages/dashboard
  (use-package page-break-lines
    :ensure t))

(when packages/dashboard
  (use-package recentf
    :ensure t
    :config
    (add-to-list 'recentf-exclude "~/org/agenda/schedule.org")
    (add-to-list 'recentf-exclude "~/org/agenda/todo.org")
    (add-to-list 'recentf-exclude "~/org/agenda/emacs.org")
    (add-to-list 'recentf-exclude "~/org/agenda/homework.org")
    (add-to-list 'recentf-exclude (concat user-emacs-directory "bookmarks"))))

(when packages/dashboard
  (use-package dashboard
    :after all-the-icons
    :after page-break-lines
    :after projectile
    :after recentf
    :hook (dashboard-mode . (lambda () (interactive) (page-break-lines-mode 1)))
    :hook (dashboard-mode . (lambda () (interactive) (display-line-numbers-mode -1)))
    :ensure t
    :init
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    (setq dashboard-items '((recents . 5)
                            (projects . 5)
                            (agenda . 5)))
    (setq dashboard-icon-type 'all-the-icons)
    (setq dashboard-page-separator "

")
    (setq dashboard-center-content t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    :config
    (dashboard-setup-startup-hook)))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . (lambda () (interactive) (rainbow-mode 1))))
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . (lambda () (interactive) (rainbow-delimiters-mode 1))))
(use-package rainbow-identifiers
  :ensure t
  :hook (prog-mode . (lambda () (interactive) (rainbow-identifiers-mode 1))))

(add-to-list 'default-frame-alist '(alpha-background . 85))

(when langs/java
  (add-hook 'java-mode-hook 'java-ts-mode))

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

(when minibuffer/ivy
  (use-package ivy
    :defer 0.1
    :diminish
    :custom
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    :config
    (ivy-mode)
    :hook (ivy-mode . (lambda ()
                        (interactive)
                        (define-key ivy-mode-map (kbd "DEL") 'ivy-backward-delete-char))))

(use-package counsel
  :after ivy
  :config
  (counsel-mode)
  (setq ivy-initial-inputs-alist nil)) ; Disable the "^" in interactive counsel commands like M-x

(use-package ivy-rich
  :after ivy
  :ensure t
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
              ("N" . evil-search-next))))

(when minibuffer/vertico
  (use-package vertico
    :ensure t
    :config
    (vertico-mode))

  (use-package marginalia
    :ensure t
    :config
    (marginalia-mode 1))

  (use-package consult
    :ensure t
    :bind (:map evil-normal-state-map
                ("/" . consult-line))))

(use-package no-littering
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))
;; also make brace auto formatting nice
;; like turn thing() {<return> into
;; thing() {
;;     stuff
;; }
;; automatically
(electric-pair-mode 1)

(when packages/autocompletion
  (use-package company
    :defer t
    :ensure t
    :config
    (global-company-mode)))

(when packages/autocompletion
  (use-package lsp-mode
    :ensure t
    :defer t
    :config
    (setq lsp-keymap-prefix "SPC c")

    ;; enable hooks for enabled languages
    (when langs/java
      (add-hook 'java-mode-hook #'lsp-deferred)
      (add-hook 'java-ts-mode-hook #'lsp-deferred))
    (when langs/haskell
      (add-hook 'haskell-mode-hook #'lsp-deferred))
    (when langs/web
      (add-hook 'js-mode-hook #'lsp-deferred)
      (add-hook 'js-ts-mode-hook #'lsp-deferred)
      (add-hook 'js-jsx-mode-hook #'lsp-deferred)
      (add-hook 'html-mode-hook #'lsp-deferred)
      (add-hook 'css-mode-hook #'lsp-deferred))
    (when langs/c
      (add-hook 'c-mode-hook #'lsp-deferred)
      (add-hook 'c++-mode-hook #'lsp-deferred))

    ;; extensions
    (when langs/haskell
      (use-package lsp-haskell
        :ensure t
        :defer t
        :after lsp-mode))

    (when langs/java
      (use-package lsp-java
        :ensure t
        :defer t
        :after lsp-mode))

    (use-package lsp-ui
      :ensure t
      :defer t
      :after lsp-mode
      :hook (lsp-mode . lsp-ui-doc-mode))))

(use-package prescient
  :ensure t
  :config
  (prescient-toggle-fuzzy 1)
  (prescient-persist-mode 1))

(when minibuffer/vertico
  (use-package vertico-prescient
    :ensure t
    :after vertico
    :after prescient
    :config
    (vertico-prescient-mode 1)))

(when minibuffer/ivy
  (use-package ivy-prescient
    :ensure t
    :after ivy
    :after prescient
    :config
    (ivy-prescient-mode 1)))

(when packages/autocompletion
  (use-package company-prescient
    :after company
    :after prescient
    :ensure t
    :config
    (company-prescient-mode 1)))

(setq indent-tabs-mode t)
(setq-default tab-width 4
              c-basic-offset 4
              c-default-style "stroustrup")
(defvaralias 'c-basic-offset 'tab-width)
(add-hook 'prog-mode-hook #'(lambda ()
                              (interactive)
                              (if (equal major-mode 'emacs-lisp-mode)
                                  (setq indent-tabs-mode nil)
                                (setq indent-tabs-mode t))))

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'abort-minibuffers)

(use-package flycheck
  :defer t
  :ensure t
  :config
  (global-flycheck-mode))

(when packages/projectile
  (use-package projectile
    :ensure t
    :config
    (projectile-mode +1))

  (use-package projectile-ripgrep
    :ensure t
    :after projectile)

  (when minibuffer/ivy
    (use-package counsel-projectile
      :ensure t
      :after '(projectile counsel)))
  (when minibuffer/vertico
    (use-package consult-projectile
      :ensure t
      :after '(projectile consult))))

(when packages/perspectives
  (use-package perspective
    :ensure t
    :config
    (setq persp-initial-frame-name "Main")
    (setq persp-mode-prefix-key "C-x x")
    (persp-mode)))

(when (and packages/perspectives packages/projectile)
(use-package persp-projectile
  :ensure t
  :after perspective
  :after projectile))

(when packages/snippets
  (use-package yasnippet
    :ensure t
    :config
    (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
    (yas-global-mode 1)))

(when packages/snippets
  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet)
  (when langs/java
    (use-package java-snippets
      :ensure t
      :after yasnippet)))

(when langs/web
  (use-package web-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist  '("\\.html$" . web-mode))
    (add-to-list 'auto-mode-alist  '("\\.css?\\'" . web-mode))
    (add-to-list 'auto-mode-alist  '("\\.js$\\'" . web-mode)))
  (use-package emmet-mode
    :ensure t
    :after web-mode
    :hook (web-mode . emmet-mode)))

(use-package evil-nerd-commenter :ensure t)

(use-package hl-todo
  :ensure t
  :hook (prog-mode . (lambda () (interactive) (hl-todo-mode 1)))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces `(("TODO"       warning bold)
                                ("FIXME"      error bold)
                                ("HACK"       font-lock-constant-face bold)
                                ("NOTE"       success bold)
                                ("DEPRECATED" font-lock-doc-face bold))))

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

(when packages/dirvish
  (use-package dirvish
    :ensure t
    :config
    (dirvish-override-dired-mode 1)
    (dirvish-peek-mode 1)))

(setq backup-directory-alist '((".*" . "~/.cache/emacs/auto-saves")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves" t)))

(setq evil-undo-system 'undo-redo)

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(when emacsOS/run-launcher
  (bugger/extern-package "SebastienWae" "app-launcher")

  ;; create a global keyboard shortcut with the following code
  ;; emacsclient -cF "((visibility . nil))" -e "(emacs-run-launcher)"
  (defun emacs-run-launcher ()
    "Create and select a frame called emacs-run-launcher which consists only of a
minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame,
 which is an emacs command that prompts you to select an app and open it in a
 dmenu like behaviour. Delete the frame after that command has exited"
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
        (delete-frame)))))

(when emacsOS/exwm
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

    ;; simulation keys. if you press one keybinding, it'll send the corresponding one to whatever application you have open
    (setq exwm-input-simulation-keys
          '(([?\C-c ?\C-c] . ?\C-c)
            ([?\C-n] . [down])
            ([?\C-p] . [up])
            ([?\C-f] . [right])
            ([?\C-b] . [left])))

    ;; Set up global key bindings.  These always work, no matter the input state!
    ;; Keep in mind that changing this list after EXWM initializes has no effect.
    (setq exwm-input-global-keys
          `(
            ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
            (,(kbd "s-r") . exwm-reset)

            ;; exit
            (,(kbd "s-C-q") . (lambda ()
                                (interactive)
                                (start-process-shell-command "killall emacs" nil "killall emacs")))

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
                                                  "snd up")))
            (,(kbd "<XF86AudioLowerVolume>") . (lambda ()
                                                 (interactive)
                                                 (start-process-shell-command
                                                  "volume-lower"
                                                  nil
                                                  "snd down")))
            (,(kbd "C-c m l") . emms-next)
            (,(kbd "C-c m h") . emms-previous)
            (,(kbd "C-c m p") . emms-pause)
            (,(kbd "C-c m r") . emms-player-mpd-update-all-reset-cache)

            ;; vterm
            (,(kbd "C-c v") . vterm-toggle)

            ;; eshell
            (,(kbd "C-c e") . (lambda ()
                                (interactive)
                                (split-window-right)
                                (eshell)))

            ;; create an emacs window
            (,(kbd "s-e") . (lambda ()
                              (interactive)
                              (split-window-right)))

            ;; brightness
            (,(kbd "<XF86MonBrightnessUp>") . (lambda ()
                                                (interactive)
                                                (start-process-shell-command
                                                 "volume-raise"
                                                 nil
                                                 "real-brightness up")))
            (,(kbd "<XF86MonBrightnessDown>") . (lambda ()
                                                  (interactive)
                                                  (start-process-shell-command
                                                   "volume-lower"
                                                   nil
                                                   "real-brightness down")))
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
                              (exwm-workspace-switch-create 9)))))))

(when emacsOS/exwm
  (defun bugger/gpg-fix ()
    (use-package pinentry
      :ensure t
      :config
      (setenv "GPG_AGENT_INFO" nil)
      (setq auth-source-debug t)

      (setq epg-gpg-program "gpg2")
      (require 'epa-file)
      (epa-file-enable)
      (setq epg-pinentry-mode 'loopback)
      (pinentry-start))

    (require 'org-crypt)
    (org-crypt-use-before-save-magic)))

;; function for renaming windows
(when emacsOS/exwm
  (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer exwm-class-name))

  (defun bugger/exwm-settings ()
    (setq exwm-workspace-number 10) ;; setting workspaces

    ;; systray
    ;; (use-package exwm-systemtray
    ;;   :config
    ;;   (exwm-systemtray-enable))

    ;; set window names
    (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
    (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)))

(when emacsOS/exwm
  (defun bugger/autostart ()
    (call-process "/bin/sh" (concat config-dir "autostart.sh"))))

(when emacsOS/exwm
  (use-package exwm
    :ensure t
    :config
    (bugger/exwm-settings)
    (bugger/gpg-fix)
    (bugger/keybindings)

    (exwm-enable)

    (when (not has-restarted)
      (bugger/autostart))))

(when emacsOS/elfeed
  (use-package elfeed
    :ensure t)
  (use-package elfeed-org
    :ensure t
    :after elfeed
    :config
    (elfeed-org))
  (use-package elfeed-goodies
    :ensure t
    :after elfeed
    :config
    (elfeed-goodies/setup)))

(when emacsOS/vterm
  (use-package vterm
    :defer t
    :ensure t
    :config
    (setq shell-file-name "/bin/zsh"
          vterm-max-scrollback 5000)))

(when emacsOS/vterm
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
                   (window-height . 0.3)))))

(when emacsOS/emms
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
    (setq mpc-host "localhost:6600")))

(when emacsOS/calendar
  (use-package calfw
    :ensure t)
  (use-package calfw-org
    :ensure
    :after calfw))

(when emacsOS/mail
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
    ;; my real email info is stored in ~/.cache/emacs/emails.el

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
    ;;                         (mu4e-trash-folder . "/[Gmail]/Trash"))))

    (load (concat user-emacs-directory "emails.el"))))

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

;; tab over the region
(when packages/evil
  (define-key evil-visual-state-map (kbd "TAB") 'indent-region)

  ;; comment/uncomment the region
  (define-key evil-visual-state-map (kbd "C-/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "C-/") 'evilnc-comment-or-uncomment-lines)

  ;; toggle tolding
  (define-key evil-normal-state-map (kbd "TAB") 'evil-toggle-fold))

;; delete a tab, not 4 spaces
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

(when packages/evil
  (use-package general
    :ensure t
    :config (general-evil-setup t)))

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(when packages/evil
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
 "b i" '(ibuffer                   :which-key "ibuffer")
 "b c" '(kill-this-buffer          :which-key "kill buffer")
 "b k" '(kill-this-buffer          :which-key "kill buffer")
 "b p" '(previous-buffer           :which-key "previous buffer")
 "b n" '(next-buffer               :which-key "next buffer")
 "b h" '(centaur-tabs-backward-tab :which-key "previous tab")
 "b l" '(centaur-tabs-forward-tab  :which-key "previous tab")
 "b r" '(revert-buffer             :which-key "reload buffer"))
(if minibuffer/vertico
    (general-define-key
     :states '(normal visual)
     "SPC b b" '(consult-buffer :which-key "switch to buffer"))
  (general-define-key
   :states '(normal visual)
   "SPC b b" '(switch-to-buffer :which-key "switch to buffer")))

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
                                                                               (find-file (concat config-dir "config.org"))))
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
 "h t" '(load-theme :which-key "load theme")
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
  (setq has-restarted t)
  (org-babel-tangle-file (concat config-dir "config.org"))
  (load-file (concat config-dir "init.el"))
  (load-file (concat config-dir "init.el")))

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
 "p" '(:ignore t :which-key "projectile")
 "p p" '(projectile-persp-switch-project :which-key "open project")
 "p c" '(projectile-compile-project :which-key "compile project")
 "p a" '(projectile-add-known-project :which-key "add project"))
(when minibuffer/vertico
  (general-define-key
   :states '(normal visual)
   "SPC /" '(consult-ripgrep :which-key "search project")
   "SPC p f" '(consult-projectile-find-file :which-key "find file")))
(when minibuffer/ivy
  (general-define-key
   :states '(normal visual)
   "SPC /" '(counsel-projectile-rg :which-key "search project")
   "SPC p f" '(counsel-projectile-find-file :which-key "find file in project")))

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
 "s i" '(persp-ibuffer :which-key "persp ibuffer")
 "s s" '(persp-switch :which-key "switch perspective")
 "s n" '(persp-next :which-key "next perspective")
 "s p" '(persp-prev :which-key "prev perspective")
 "s a" '(persp-add-buffer :which-key "add buffer to perspesctive")
 "s A" '(persp-set-buffer :which-key "brgin buffer to perspective")
 "s r" '(persp-remove :which-key "remove buffer from perspective")
 "s k" '(persp-kill :which-key "kill perspective")
 "s K" '(persp-kill-others :which-key "kill other perspectives"))
(when minibuffer/vertico
  (general-define-key
   :states '(normal visual)
   "SPC s b" '(persp-switch-to-buffer)))
(when minibuffer/ivy)
  (general-define-key
   :states '(normal visual)
   "SPC s b" '(persp-counsel-switch-buffer)))

(setq gc-cons-threshold (* 2 1024 1024))
(server-start)
