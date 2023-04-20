;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.local/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

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

; Nice to have pager-like scrolling
(global-set-key (kbd "C-j") #'(lambda ()
								(interactive)
								(evil-scroll-down 1)))
(global-set-key (kbd "C-k") #'(lambda ()
								(interactive)
								(evil-scroll-up 1)))

; Dired keybindings
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file)) ; use dired-open-file instead if using dired-open package

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
(setq initial-buffer-choice "~/.config/doom/start.org")
(define-minor-mode start-mode
  "Defines a custom mode for the start page"
  :lighter " start")
  ;:keymap (let ((map (make-sparse-keymap)))
	    ;(evil-define-key 'normal start-mode-map
	      ;(kbd "e") #'(lambda () (interactive) (counsel-find-file "~/.config/emacs/init.el"))
	      ;(kbd "z") #'(lambda () (interactive) (counsel-find-file "~/.config/zsh/.zshrc"))
	      ;(kbd "p") #'(lambda () (interactive) (counsel-find-file "~/.config/polybar/config.ini"))
	      ;(kbd "a") #'(lambda () (interactive) (counsel-find-file "~/.config/alacritty/alacritty.yml"))
	      ;(kbd "x") #'(lambda () (interactive) (counsel-find-file "~/.config/xmonad/xmonad.hs"))
	      ;(kbd "f") 'counsel-find-file
	      ;(kbd "d") 'dired)
	  ;map))

(add-hook 'start-mode-hook 'read-only-mode)
(provide 'start-mode)
(setq org-link-elisp-skip-confirm-regexp "\\`find-file*\\'")

;; Org Mode

; Bullets
(add-hook 'org-mode-hook #'(lambda ()
	(interactive)
	(org-bullets-mode 1)))
(setq org-hide-leading-stars t)

;(defun some-guy/org-colors-molokai ()
;(dolist
	;(face
	 ;'((org-level-1       1.7 "#fb2874" ultra-bold)
	   ;(org-level-2       1.6 "#fd971f" extra-bold)
	   ;(org-level-3       1.5 "#9c91e4" bold)
	   ;(org-level-4       1.4 "#268bd2" semi-bold)
	   ;(org-level-5       1.3 "#e74c3c" normal)
	   ;(org-level-6       1.2 "#b6e63e" normal)
	   ;(org-level-7       1.1 "#66d9ef" normal)
	   ;(org-level-8       1.0 "#e2c770" normal)
	   ;(org-table         1.0 "#d4d4d4" normal)
	   ;(org-table-header  1.0 "#d4d4d4" normal)
	   ;(org-link          1.3 "#9c91e4" normal)))
	;(set-face-attribute (nth 0 face) nil :family 'JetBrainsMono :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
	;(set-face-attribute 'org-table nil :family 'JetBrainsMono :weight 'normal :height 1.0 :foreground "#d4d4d4"))
;(some-guy/org-colors-molokai)

;; Dired

; Open things nicely
(setq dired-open-extensions '(
                                                          ("gif" . "mpv")
							  ("jpg" . "feh")
							  ("png" . "feh")
							  ("mkv" . "mpv")
							  ("mp4" . "mpv")
							  ("mp3" . "mpv")))
