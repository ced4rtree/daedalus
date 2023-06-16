(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15))
(setq doom-unicode-font (font-spec :family "MesloLGS NF" :size 13))

(setq highlight-indent-guides-method 'character)

(beacon-mode 1)

;(eval-after-load "zone"
  ;(zone-when-idle 120))

(eval-after-load "zone"
  '(unless (memq 'zone-nyan (append zone-programs nil))
     (setq zone-programs
           (vconcat zone-programs [zone-nyan]))))
(eval-after-load "zone"
  '(unless (memq 'zone-pgm-sl (append zone-programs nil))
     (setq zone-programs
           (vconcat zone-programs [zone-pgm-sl]))))
(eval-after-load "zone"
  '(unless (memq 'zone-rainbow (append zone-programs nil))
     (setq zone-programs
           (vconcat zone-programs [zone-rainbow]))))

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

(global-tree-sitter-mode)
; (add-hook 'prog-mode-hook) on literally anything causes the server start to just not happen, so I have to do it like this instead
(add-hook 'c-mode-hook #'(lambda () (interactive) (tree-sitter-hl-mode 1)))
(add-hook 'java-mode-hook #'(lambda () (interactive) (tree-sitter-hl-mode 1)))
(add-hook 'rust-mode-hook #'(lambda () (interactive) (tree-sitter-hl-mode 1)))
(add-hook 'c++-mode-hook #'(lambda () (interactive) (tree-sitter-hl-mode 1)))
(add-hook 'sh-mode-hook #'(lambda () (interactive) (tree-sitter-hl-mode 1)))
(add-hook 'javascript-mode-hook #'(lambda () (interactive) (tree-sitter-hl-mode 1)))

(if (not (daemonp))
	 (centaur-tabs-mode)

  (defun centaur-tabs-daemon-mode (frame)
	 (unless (and (featurep 'centaur-tabs) (centaur-tabs-mode-on-p))
		(run-at-time nil nil (lambda () (centaur-tabs-mode)))))
  (add-hook 'after-make-frame-functions #'centaur-tabs-daemon-mode))

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

(general-define-key
 :states '(normal visual)
 :prefix "SPC d"
 "d" '(dired :which-key "Open dired")
 "j" '(dired-jump :which-key "Open dired in current directory"))

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

(general-define-key
 :states '(normal visual)
 :prefix "SPC o"
 "C" '(cfw:open-org-calendar :which-key "Open org calendar")
 "a c" '(cfw:open-org-calendar :which-key "Open org calendar"))

(setq-default c-default-style "k&r"
	      c-indentation-style "k&r"
	      c-basic-offset 4
	      tab-width 4
	      js2-basic-offset 4
	      indent-tabs-mode t)

(defvaralias 'c-basic-offset 'tab-width)
(add-hook 'cc-mode-hook (lambda () (setq-local tab-width 4)))
(add-hook 'cc-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'cc-mode-hook (lambda () (setq-local c-basic-offset 4)))
(add-hook 'cc-mode-hook (lambda () (setq c-basic-offset 4)))
(add-hook 'js-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'js-mode-hook (lambda () (setq js2-basic-offset 4)))
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <indent-for-tab-command>") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <c-indent-line-or-region>") 'tab-to-tab-stop)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
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
(setq org-agenda-start-on-weekday 0)

(setq org-ellipsis " ▼ ")
(setq org-directory "~/org")

;(require 'emms-player-mpd)
(emms-all)
(setq emms-player-list '(emms-player-mpd)
      emms-info-functions '(emms-info-mpd emms-info-native)
      emms-player-mpd-server-name "localhost"
      emms-player-mpd-server-port "6600"
      emms-player-mpd-music-directory (concat (getenv "HOME") "/music"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC e"
 "e" '(emms-smart-browse :which-key "Open emms")
 "s" '(emms-shuffle :which-key "Shuffle the playlist")
 "h" '(emms-next :which-key "Play the next song")
 "l" '(emms-previous :which-key "Play the previous song")
 "SPC" '(emms-pause :which-key "Pause the music")
 "r" '(emms-random :which-key "Play a random song")
 "f" '(emms-play-file :which-key "Select a song to play"))
