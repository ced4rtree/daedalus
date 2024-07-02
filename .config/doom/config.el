;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "UbuntuSans Nerd Font" :size 14))

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(use-package! eldoc-box
  :after eglot
  :after eldoc
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(vertico-posframe-mode t)

(map! :after centaur-tabs
      :map ('override evil-normal-state-map evil-visual-state-map)
      :leader
      :prefix "TAB"
      "h" #'centaur-tabs-backward
      "l" #'centaur-tabs-forward)

(map! :after dired
      :leader
      "d j" #'dired-jump)
(after! dired
  (evil-define-key '(normal visual) dired-mode-map
    (kbd "l") #'dired-find-file
    (kbd "h") #'dired-up-directory))

(setq org-agenda-files '("~/org/agenda/"))

;; scrolling
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(pixel-scroll-precision-mode t)

;; calfw binding
(map! :after calendar
      :leader
      "o a c" #'cfw:open-org-calendar
      "o C" #'cfw:open-org-calendar)
