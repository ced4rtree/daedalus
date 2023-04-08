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

(defun betterInstall (package)
  (interactive)
  (unless (package-installed-p package)
	(package-refresh-contents)
	(package-install package)))

;; Packages
(betterInstall 'use-package)
(betterInstall 'evil)
(evil-mode)
(betterInstall 'which-key)
(use-package which-key
	:ensure t
	:init
	(which-key-mode))
(betterInstall 'doom-themes)
(betterInstall 'smartparens)
(betterInstall 'rainbow-mode)
(betterInstall 'rainbow-delimiters)
(betterInstall 'rainbow-identifiers)
(betterInstall 'lsp-mode)
(betterInstall 'lsp-haskell)
(betterInstall 'lsp-treemacs)
(betterInstall 'lsp-intellij)
(betterInstall 'org-bullets)
(betterInstall 'key-chord)
(betterInstall 'vterm)
(betterInstall 'treemacs)

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
	:init (smex-initialize)
	:bind
	("M-x" . smex))

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
(define-key evil-normal-state-map (kbd "<remap> <evil-jump-forward>") 'ibuffer-jump)
(define-key evil-normal-state-map (kbd "<remap> <evil-scroll-page-up>") 'pop-to-buffer)
(define-key evil-normal-state-map (kbd "<remap> <evil-scroll-page-down>") 'find-file)

; kill the current buffer with 'q'
(define-key evil-normal-state-map (kbd "<remap> <evil-record-macro>") #'(lambda ()
																		 (interactive)
																		 (when (buffer-modified-p)
																		   (when (y-or-n-p "Buffer modified. Save?")
																			 (save-buffer)))
																		 (kill-buffer (buffer-name))))
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

; Better scrolling
(define-key evil-normal-state-map (kbd "<remap> <electric-newline-and-maybe-indent>") #'(lambda ()
																						  (interactive)
																						  (evil-ret 1)
																						  (evil-scroll-line-down 1)))
(define-key evil-normal-state-map (kbd "<remap> <kill-line>") #'(lambda ()
																  (interactive)
																  (evil-ret -1)
																  (evil-scroll-line-up 1)))

;; Doom-like bindings
(require 'key-chord)
(add-to-list 'load-path "/home/some-guy/.config/emacs/plugins")
(require 'space-chord)
(key-chord-mode 1)
(add-hook 'org-bullets-mode-hook #'(lambda ()

	(space-chord-define evil-normal-state-map "." 'find-file)
	(space-chord-define evil-normal-state-map "i" 'ibuffer)
	(space-chord-define evil-normal-state-map "b" 'pop-to-buffer-same-window)
	(space-chord-define evil-normal-state-map "B" 'pop-to-buffer)
	(space-chord-define evil-normal-state-map "q" '(lambda ()
													  ;(interactive)
													  (when (buffer-modified-p)
														(when (y-or-n-p "Buffer modified. Save?")
														  (save-buffer)))
													  (evil-window-delete)))
	(space-chord-define evil-normal-state-map "w" 'evil-window-next)
	(space-chord-define evil-normal-state-map "v" 'evil-window-vsplit)
	(space-chord-define evil-normal-state-map "n" 'evil-window-new)))
; space-chord just kinda decides to not work sometimes, but restarting key-chord-mode fixes it
(global-set-key (kbd "C-<return>") #'(lambda () (interactive) (key-chord-mode 1)))

;; Autocompletion
(require 'lsp-mode)
  (setq lsp-keymap-prefix "c-l")
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'cc-mode-hook #'lsp)
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'sh-mode-hook #'lsp)
  (add-hook 'haskell-mode-hook #'lsp)

(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))


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
  :lighter " start")

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

;; Pretty theme
(use-package doom-themes
	:ensure t)
(add-hook 'org-bullets-mode-hook (lambda () (load-theme 'doom-molokai)))
 ;'(default ((t (:inherit nil :extend nil :stipple nil :background "#1e1e1e" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "ADBO" :family "JetBrainsMono")))))

; Spaceline
(use-package spaceline :ensure t
  :config
  (use-package spaceline-config
    :config
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-buffer-encoding-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (setq powerline-default-separator 'rounded)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-define-segment line-column
      "The current line and column numbers."
      "l:%l c:%2c")
    (spaceline-define-segment time
      "The current time."
      (format-time-string "%H:%M"))
    (spaceline-define-segment date
      "The current date."
      (format-time-string "%h %d"))
    (spaceline-toggle-time-on)
    (spaceline-emacs-theme 'date 'time)))

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
   '(helm ivy key-chord hasklig-mode haskell-emacs-base yuck-mode powerline-evil nyan-mode flymake-elisp-config spaceline haskell-mode haskell-emacs emms mu4e-conversation mu4easy org-bullets org-present centaur-tabs 2048-game typit pacmacs lsp-intellij tree-sitter-langs tree-sitter-indent tree-sitter company-box company-jedi ibuffer-tramp company-fuzzy company-irony lsp-ivy magit treemacs-evil company flycheck lsp-java yasnippet-snippets yasnippet el-autoyas fd-dired dired-ranger dired-rainbow use-package package+))
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1c1e1f" :foreground "#d6d6d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "JB" :family "JetBrains Mono")))))
