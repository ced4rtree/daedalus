(setq server-socket-dir (substitute-in-file-name "$HOME/.config/emacs/server-dir"))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

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

(betterInstall 'spacemacs-theme)

(betterInstall 'smartparens)

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
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'cpp-mode-hook #'smartparens-mode)
(add-hook 'cxx-mode-hook #'smartparens-mode)
(add-hook 'cc-mode-hook #'smartparens-mode)
(add-hook 'java-mode-hook #'smartparens-mode)

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)

;; Lsp
(setq lsp-keymap-prefix "c-l")
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'cc-mode-hook #'lsp)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'sh-mode-hook #'lsp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(ibuffer-tramp company-fuzzy company-irony lsp-ivy magit treemacs-evil company flycheck lsp-java yasnippet-snippets yasnippet el-autoyas fd-dired dired-ranger dired-rainbow use-package package+))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "ADBO" :family "JetBrains Mono")))))
