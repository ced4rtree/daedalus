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
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'cpp-mode-hook #'smartparens-mode)
(add-hook 'cxx-mode-hook #'smartparens-mode)
(add-hook 'cc-mode-hook #'smartparens-mode)
(add-hook 'java-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

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
(define-key evil-normal-state-map (kbd "<remap> <evil-record-macro>") #'(lambda () (
	(if (buffer-modified-p)
	    (message "Buffer modified. Save?")
		(if (y-or-n-p)
	      (Save-buffer)))
	(kill-buffer (buffer-name)))))
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;; Autocompletion
(require 'lsp-mode)
  (setq lsp-keymap-prefix "c-l")
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'cc-mode-hook #'lsp)
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'sh-mode-hook #'lsp)

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

;; Pretty theme
(use-package doom-themes
	:ensure t)
(add-hook 'org-bullets-mode-hook (lambda () (load-theme 'doom-dark+)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-dark+))
 '(custom-safe-themes
   '("adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "d537a9d42c6f5349d1716ae9be9a0645cc168f7aff2a8353819d570e5d02c0b3" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "2700786ab07967d6e952285dfa18341382fb8a6a7f70d667778f2ce489436a17" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(haskell-mode haskell-emacs emms mu4e-conversation mu4easy org-bullets org-present centaur-tabs 2048-game typit pacmacs lsp-intellij tree-sitter-langs tree-sitter-indent tree-sitter company-box company-jedi ibuffer-tramp company-fuzzy company-irony lsp-ivy magit treemacs-evil company flycheck lsp-java yasnippet-snippets yasnippet el-autoyas fd-dired dired-ranger dired-rainbow use-package package+))
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
