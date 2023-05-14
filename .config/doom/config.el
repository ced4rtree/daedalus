(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetbrainsMono Nerd Font" :size 15))
(setq doom-unicode-font (font-spec :family "MesloLGS NF" :size 13))

(setq highlight-indent-guides-method 'character)

(beacon-mode 1)

(setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "The Modal Text Editor With More Than Vim")
  (setq dashboard-startup-banner "~/.config/emacs/dash-text.txt")
  (setq dashboard-items '((recents . 5)
						  (bookmarks . 5)
						  (agenda . 5)))

(dashboard-setup-startup-hook)
(add-hook 'gnus-started-hook 'revert-buffer)

(define-minor-mode start-mode
  "Provide functions for custom start page."
  :lighter " start"
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key 'normal start-mode-map
              (kbd "e") '(lambda () (interactive) (find-file "~/.config/doom/config.org"))
              (kbd "z") '(lambda () (interactive) (find-file "~/.config/zsh/.zshrc"))
              (kbd "p") '(lambda () (interactive) (find-file "~/.config/polybar/config.ini"))
              (kbd "x") '(lambda () (interactive) (find-file "~/.config/xmonad/xmonad.hs"))
              (kbd "A") 'org-agenda
              (kbd "s") 'org-show-todo-tree
              (kbd "f") 'find-file
              (kbd "d") 'dired
              (kbd "l") 'dashboard-return)
          map))

(add-hook 'start-mode-hook 'read-only-mode)
(provide 'start-mode)
(add-hook 'dashboard-mode-hook 'start-mode)

(global-set-key (kbd "C-j") #'(lambda ()
								(interactive)
								(evil-scroll-down 1)))
(define-key evil-normal-state-map (kbd "<remap> <org-return-and-maybe-indent") #'(lambda ()
								(interactive)
								(evil-scroll-down 1)))
(global-set-key (kbd "C-k") #'(lambda ()
								(interactive)
								(evil-scroll-up 1)))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-file)) ; use dired-open-file instead if using dired-open package

(with-eval-after-load 'ibuffer
  (evil-define-key 'normal ibuffer-mode-map (kbd "l") 'ibuffer-visit-buffer))

(global-set-key (kbd "DEL") 'backward-delete-char)
(define-key ivy-mode-map (kbd "DEL") 'ivy-backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

(define-key evil-normal-state-map (kbd "q") (lambda ()
  (interactive)
  (when (buffer-modified-p)
	(when (y-or-n-p "Buffer modified. Save?")
	  (save-buffer)))
  (kill-buffer (buffer-name))))
(define-key evil-normal-state-map (kbd "Q") (lambda ()
  (interactive)
  (when (buffer-modified-p)
	(when (y-or-n-p "Buffer modified. Save?")
	  (save-buffer)))
  (kill-buffer-and-window)))

; Need named function for which-key
(defun bugger/edit-src ()
  (interactive)
  (if (org-src-edit-buffer-p)
      (org-edit-src-exit)
    (org-edit-special)))
(general-define-key
 :states '(normal visual)
 "SPC m '" '(bugger/edit-src :which-key "Edit a code block"))

(setq-default c-default-style "stroustrup"
	      c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode 1)
(add-hook 'haskell-indentation-mode-hook (lambda () (interactive) (setq-default indent-tabs-mode 1)))
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <indent-for-tab-command>") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <c-indent-line-or-region>") 'tab-to-tab-stop)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
(setq org-hide-leading-stars t)

; Pretty colors and sizes for org mode
(defun bugger/org-colors-doom-molokai ()
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

; thanks dt for this one
(defun dt/org-colors-doom-one ()
  "Enable Doom One colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#51afef" ultra-bold)
         (org-level-2 1.6 "#c678dd" extra-bold)
         (org-level-3 1.5 "#98be65" bold)
         (org-level-4 1.4 "#da8548" semi-bold)
         (org-level-5 1.3 "#5699af" normal)
         (org-level-6 1.2 "#a9a1e1" normal)
         (org-level-7 1.1 "#46d9ff" normal)
         (org-level-8 1.0 "#ff6c6b" normal)))
    (set-face-attribute (nth 0 face) nil :family 'JetBrainsMono :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :family 'JetBrainsMono :weight 'normal :height 1.0 :foreground "#bfafdf"))

(dt/org-colors-doom-one)

(setq org-roam-v2-ack t)
(setq org-roam-directory "~/org/notes")
(setq org-roam-completion-everywhere t)
(org-roam-db-autosync-enable)

(setq org-agenda-files (list "~/org/agenda/todo.org"
							 "~/org/agenda/homework.org"
							 "~/org/agenda/emacs.org"
							 "~/org/agenda/schedule.org"))
(setq org-agenda-weekend-days (list 5 6))

(setq org-ellipsis " ▼ ")
(setq org-directory "~/org")
