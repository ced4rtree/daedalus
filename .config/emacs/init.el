  (require 'package)
  (setq package-user-dir "~/.config/emacs/.local/elpa")
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (setq use-package-always-ensure t)

(add-to-list 'default-frame-alist
             '(font . "AnonymicePro Nerd Font Mono-15"))
(use-package dired-all-the-icons
  :after dired
  :hook (dired-mode . dired-all-the-icons-mode))

(use-package highlight-indent-guides
  :defer t
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

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
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type 0
		doom-modeline-height 35)
  (when (daemonp)
    (setq doom-modeline-icon t))
  :config
  
  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook 'ef-themes-post-load-hook #'doom-modeline-refresh-bars))

  (global-display-line-numbers-mode 1)

(global-hl-line-mode)

  (use-package ef-themes
    :ensure t
    :config (load-theme 'ef-trio-dark t))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq scroll-conservatively 101)

  (use-package projectile
    :config
    (projectile-mode +1))

(use-package all-the-icons
  :if (display-graphic-p))

  (use-package page-break-lines
    :config (global-page-break-lines-mode))

  (use-package recentf
    :config
    (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/schedule.org"))
    (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/todo.org"))
    (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/emacs.org"))
    (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/homework.org"))
    (add-to-list 'recentf-exclude (concat (getenv "HOME") "/.config/emacs/bookmarks")))

    (use-package dashboard
      :after all-the-icons
      :after page-break-lines
      :after projectile
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

  (use-package org-tempo
    :ensure nil)

  (use-package org-auto-tangle
    :ensure t
    :defer t
    :hook (org-mode . org-auto-tangle-mode))

  (add-hook 'org-mode-hook 'org-indent-mode)

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

;; a better org agenda interface
(use-package calfw)
(use-package calfw-org :after calfw)

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
    :defer t
    :config
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
(use-package lsp-haskell
  :defer t
  :after lsp-mode)
(use-package lsp-treemacs
  :defer t
  :after lsp-mode)
(use-package lsp-java
  :defer t
  :after lsp-mode)

(use-package flycheck
  :defer t
  :config
  (global-flycheck-mode))

(use-package vterm :defer t)

(use-package treemacs :defer t)
(use-package treemacs-evil :after (treemacs evil))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-all-the-icons :after treemacs)

(setq evil-undo-system 'undo-redo)

  (use-package evil
    :init
	(setq evil-want-keybinding nil)
    (evil-mode 1)
    (setq evil-undo-system 'undo-redo))

(use-package evil-collection
	:after evil
	:defer t
	:config
	(setq evil-collection-mode-list '(dashboard dired ibuffer search agenda))
	(evil-collection-init))

  (use-package general
    :ensure t
    :init (general-evil-setup t))

  (use-package which-key
    :ensure t
    :config (which-key-mode 1))

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
   "b"   '(:ignore t       :which-key "buffer")
   "b b" '(buffer-menu     :which-key "buffer menu")
   "b i" '(ibuffer         :which-key "ibuffer")
   "b c" '(kill-buffer     :which-key "kill buffer")
   "b k" '(kill-buffer     :which-key "kill buffer")
   "b p" '(previous-buffer :which-key "previous buffer")
   "b n" '(next-buffer     :which-key "next buffer"))

  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
  (with-eval-after-load 'ibuffer
    (evil-define-key 'normal ibuffer-mode-map (kbd "l") 'ibuffer-visit-buffer))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file))

(with-eval-after-load "evil"
  (add-hook 'dashboard-mode-hook #'(lambda ()
								   (interactive)
								   (evil-local-set-key 'normal (kbd "r") 'dashboard-jump-to-recents)
								   (evil-local-set-key 'normal (kbd "l") 'dashboard-return)
								   (evil-local-set-key 'normal (kbd "e") #'(lambda ()
																			 (interactive)
																			 (find-file "~/.config/emacs/config.org")))
								   (evil-local-set-key 'normal (kbd "x") #'(lambda ()
																			 (interactive)
																			 (find-file "~/.config/xmonad/xmonad.org")))
								   (evil-local-set-key 'normal (kbd "p") #'(lambda ()
																			 (interactive)
																			 (find-file "~/.config/polybar/config.ini"))))))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "o"     '(:ignore t :which-key "org")
 "o a"   '(:ignore t :which-key "org agenda")
 "o a c" '(cfw:open-org-calendar :which-key "open org calendar")
 "o C"   '(cfw:open-org-calendar :which-key "open org calendar")
 "o a a" '(org-agenda :which-key "open org agenda")
 "o a t" '(org-agenda-todo :which-key "open todo list"))

(defun bugger/reload ()
  (interactive)
  (org-babgel-tangle-file "~/.config/emacs/config.org")
  (load-file "~/.config/emacs/init.el")
  (load-file "~/.config/emacs/init.el"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "h" '(:ignore t :which-key "help")
 "h r" '(:ignore t :which-key "reload")
 "h r r" '(bugger/reload :which-key "reload emacs")
 "h v" '(describe-variable :which-key "describe variable")
 "h f" '(describe-function :which-key "describe function"))

(setq gc-cons-threshold (* 2 1024 1024))
