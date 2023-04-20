(setq server-socket-dir (substitute-in-file-name "$HOME/.config/emacs/server-dir"))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

; Evil mode with evil in every buffer
(setq evil-want-keybinding nil)
(use-package evil
	:init
	(evil-mode))
(use-package evil-collection
	:after evil
	:config
	(setq evil-collection-mode-list '(dashboard dired ibuffer))
	(evil-collection-init))

; General lets us use space for a prefix, very ergonomic
(use-package general
	:config
	(general-evil-setup t))

; Display some help for forgetting keybindings
(use-package which-key
	:ensure t
	:init
	(which-key-mode))

; Autocompletion
(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))
(use-package lsp-mode)
(use-package lsp-haskell)
(use-package lsp-treemacs)
(use-package lsp-java)
(use-package lsp-ui)

; Dired
(use-package all-the-icons-dired)
(use-package dired-open)
(use-package peep-dired)

; Prettification
(use-package doom-themes)
(use-package smartparens)
(use-package rainbow-mode)
(use-package rainbow-delimiters)
(use-package rainbow-identifiers)
(use-package all-the-icons)
(use-package org-bullets)
(use-package doom-modeline :ensure t)

; Misc
(use-package vterm)
(use-package treemacs)
(use-package sudo-edit)

;; Rainbows
(require 'rainbow-mode)
(require 'rainbow-delimiters)
(require 'rainbow-identifiers)

(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ido-vertical-mode
	:ensure t
	:init
	(ido-vertical-mode 1))

(use-package smex
	:ensure t
	:init (smex-initialize))

;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode)

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq auto-window-vscroll nil)
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(beacon-mode 1)

;; Keybindings
; kill the current buffer with 'q'
(define-key evil-normal-state-map (kbd "<remap> <evil-record-macro>") #'(lambda ()
																		 (interactive)
																		 (when (buffer-modified-p)
																		   (when (y-or-n-p "Buffer modified. Save?")
																			 (save-buffer)))
																		 (kill-buffer (buffer-name))))

(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

(defun bugger/killBuffer ()
	(interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "Buffer modified. Save?")
	  (save-buffer)))
  (kill-buffer (buffer-name)))
(defun bugger/killBufferAndWindow ()
	(interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p "Buffer modified. Save?")
	  (save-buffer)))
  (kill-buffer (buffer-name)))

; Doom-like bindings
(nvmap :prefix "SPC"
	   ; Buffers
	   "b i" '(ibuffer :which-key "Ibuffer")
	   "b c" '(bugger/killBuffer :which-key "Close the current buffer")
	   "b k" '(bugger/killBufferAndWindow :which-key "Close the current buffer and window")
	   "b b" '(pop-to-buffer :which-key "Open a buffer in a new window")

	   ; Windows
	   "w v" '(evil-window-vsplit :which-key "Open a vertical split")
	   "w w" '(evil-window-next :which-key "Switch to the next window")
	   "w n" '(evil-window-new :which-key "Open a horizontal split")
	   "w c" '(evil-window-delete :which-key "Close the current window")
	   "w k" '(bugger/killBufferAndWindow :which-key "Close the current buffer and window")

	   ; Dired
	   "d d" '(dired :which-key "Open dired")
	   "d j" '(dired-jump :which-key "Open dired in the current directory")
	   "d p" '(peep-dired :which-key "Activate peep-dired")

	   ; Files
	   "."   '(find-file :which-key "Open a file")
	   "f s" '(save-buffer :which-key "Save file")
	   "f r" '(recentf-open-files :which-key "List recent files to open")
	   "f u" '(sudo-edit-find-file :which-key "Find file as root")
	   "f U" '(sudo-edit :which-key "Edit as root")
	   )

; Nice to have pager-like scrolling
(global-set-key (kbd "C-j") #'(lambda ()
								(interactive)
								(evil-scroll-down 1)))
(global-set-key (kbd "C-j") #'(lambda ()
								(interactive)
								(evil-scroll-up 1)))

; Dired keybindings
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

; Miscellaneous keybindings
(global-set-key (kbd "M-x") 'smex)

;; LSP
(setq lsp-keymap-prefix "c-l")
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'cc-mode-hook #'lsp)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'sh-mode-hook #'lsp)
(add-hook 'haskell-mode-hook #'lsp)

; Performance improvement
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1 mb
(setq lsp-use-plists t)
(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil)

;; Tabs
(setq-default c-default-style "stroustrup"
	      c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)
(defvaralias 'c-basic-offset 'tab-width)
(add-hook 'haskell-indentation-mode-hook #'(lambda () (interactive) (setq-default indent-tabs-mode t)))
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <indent-for-tab-command>") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <c-indent-line-or-region>") 'tab-to-tab-stop)

;; Movement
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(setq-default evil-cross-lines t)

;; Start page
(setq initial-buffer-choice "~/.config/emacs/start.org")
(define-minor-mode start-mode
  "Defines a custom mode for the start page"
  :lighter " start"
  :keymap (let ((map (make-sparse-keymap)))
          ;;(define-key map (kbd "M-z") 'eshell)
            (evil-define-key 'normal start-mode-map
              (kbd "e") #'(lambda () (interactive) (find-file "~/.config/emacs/init.el"))
              (kbd "z") #'(lambda () (interactive) (find-file "~/.config/zsh/.zshrc"))
              (kbd "p") #'(lambda () (interactive) (find-file "~/.config/polybar/config.ini"))
              (kbd "a") #'(lambda () (interactive) (find-file "~/.config/alacritty/alacritty.yml"))
              (kbd "x") #'(lambda () (interactive) (find-file "~/.config/xmonad/xmonad.hs"))
              (kbd "f") 'find-file
              (kbd "d") 'dired)
          map))

(add-hook 'start-mode-hook 'read-only-mode)
(provide 'start-mode)
(setq org-link-elisp-skip-confirm-regexp "\\`find-file*\\'")
;(define-key start-mode-map (kbd "e") '(lambda () (find-file (concat (getenv "HOME") "/.config/emacs/init.el"))))
;(define-key start-mode-map (kbd "f") 'find-file)

;; Org Mode

; Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook #'(lambda ()
							(interactive)
							(local-unset-key (kbd "RET"))
							(local-set-key (kbd "RET") (org-open-at-point))))

; Some QOL for link handling
(setq-default org-link-elisp-confirm-function nil)
(add-hook 'org-mode-hook #'(lambda () (setq-default org-return-follows-link t)))

(defun some-guy/org-colors-molokai ()
(dolist
	(face
	 '((org-level-1       1.7 "#fb2874" ultra-bold)
	   (org-level-2       1.6 "#fd971f" extra-bold)
	   (org-level-3       1.5 "#9c91e4" bold)
	   (org-level-4       1.4 "#268bd2" semi-bold)
	   (org-level-5       1.3 "#e74c3c" normal)
	   (org-level-6       1.2 "#b6e63e" normal)
	   (org-level-7       1.1 "#66d9ef" normal)
	   (org-level-8       1.0 "#e2c770" normal)
	   (org-table         1.0 "#d4d4d4" normal)
	   (org-table-header  1.0 "#d4d4d4" normal)
	   (org-link          1.3 "#9c91e4" normal)))
	(set-face-attribute (nth 0 face) nil :family 'JetBrainsMono :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
	(set-face-attribute 'org-table nil :family 'JetBrainsMono :weight 'normal :height 1.0 :foreground "#d4d4d4"))
(some-guy/org-colors-molokai)

;; Dired
; Previews
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
; Pretty icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
; Open things nicely
(setq dired-open-extensions '(("gif" . "mpv")
							  ("jpg" . "feh")
							  ("png" . "feh")
							  ("mkv" . "mpv")
							  ("mp4" . "mpv")
							  ("mp3" . "mpv")))

;; Pretty theme
(use-package doom-themes
	:ensure t)
(add-hook 'org-bullets-mode-hook (lambda () (load-theme 'doom-molokai)))
 ;'(default ((t (:inherit nil :extend nil :stipple nil :background "#1e1e1e" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "ADBO" :family "JetBrainsMono")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-dark+))
 '(custom-safe-themes
   '("be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(evil-undo-system 'undo-redo)
 '(org-return-follows-link t)
 '(package-selected-packages
   '(spaceline-config which-key vterm use-package sudo-edit spaceline smex smartparens rainbow-mode rainbow-identifiers rainbow-delimiters peep-dired org-bullets key-chord ido-vertical-mode general evil-collection emojify-logos doom-themes dired-open company beacon all-the-icons-dired))
 '(warning-suppress-types '((use-package) (use-package) (lsp-mode) (lsp-mode) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1c1e1f" :foreground "#d6d6d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "JB" :family "JetBrains Mono")))))
