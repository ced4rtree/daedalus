(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(setq evil-want-keybinding nil)
(use-package evil
	:init
	(evil-mode))

(setq read-process-output-max (* 1024 1024)) ;; 1 mb

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)

(require 'rainbow-mode)
(require 'rainbow-delimiters)
(require 'rainbow-identifiers)

(add-hook 'prog-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode 1)))
(add-hook 'prog-mode-hook (lambda () (rainbow-identifiers-mode 1)))

(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 120
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 120
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

(use-package doom-themes
  :hook (after-init . (lambda () (load-theme 'doom-one t))))
(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1)
  (setq doom-modeline-height 35
        doom-dark+-blue-modeline t))

(setq native-comp-async-report-warnings-errors nil)

(use-package highlight-indent-guides
  :defer t)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(use-package recentf :ensure t) ; just required to remove some stuff from the 'recent files' section in dashboard

(use-package dashboard
  :ensure t
  :after recentf
  :init
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/schedule.org"))
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/todo.org"))
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/emacs.org"))
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/org/agenda/homework.org"))
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/.config/emacs/bookmarks"))

  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "The Modal Text Editor With More Than Vim")
  (setq dashboard-startup-banner "~/.config/emacs/dash-text.txt")
  (setq dashboard-items '((recents . 5)
						  (bookmarks . 5)
						  (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(define-minor-mode start-mode
  "Provide functions for custom start page."
  :lighter " start"
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key 'normal start-mode-map
              (kbd "e") '(lambda () (interactive) (find-file "~/.config/emacs/config.org"))
              (kbd "z") '(lambda () (interactive) (find-file "~/.config/zsh/.zshrc"))
              (kbd "p") '(lambda () (interactive) (find-file "~/.config/polybar/config.ini"))
              (kbd "x") '(lambda () (interactive) (find-file "~/.config/xmonad/xmonad.hs"))
              (kbd "a") 'org-agenda
			  (kbd "s") 'org-show-todo-tree
              (kbd "f") 'find-file
              (kbd "d") 'dired
			  (kbd "l") 'dashboard-return)
          map))

(add-hook 'start-mode-hook 'read-only-mode)
(provide 'start-mode)
(add-hook 'dashboard-mode-hook 'start-mode)

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(use-package nyan-mode :defer t)
(setq nyan-animate-nyancat t)
(setq nyan-wavy-trail t)
(setq nyan-bar-length 80)
(add-hook 'prog-mode-hook (lambda () (nyan-mode 1)))

(use-package zone
  :defer t
  :config (zone-when-idle 120))

(use-package zone-sl :defer t)
(use-package zone-rainbow :defer t)
(use-package zone-nyan :defer t)

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

(use-package tree-sitter :defer t)
(use-package tree-sitter-langs
  :defer t
  :after tree-sitter)
(global-tree-sitter-mode)

(use-package all-the-icons :defer t) ; Neat little icons everywhere
(use-package all-the-icons-dired :defer t) ; And in dired too
(use-package beacon ; Neat little light for your cursor
  :defer t
  :config (beacon-mode 1))
(global-hl-line-mode 1)

(use-package dired-open :defer t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq dired-open-extensions '(("gif" . "mpv")
							  ("jpg" . "feh")
							  ("png" . "feh")
							  ("mkv" . "mpv")
							  ("mp4" . "mpv")
							  ("mp3" . "mpv")))

(use-package company
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'company-mode)))

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

(use-package smartparens
  :defer t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package vterm :defer t)
(use-package treemacs :defer t)
(use-package sudo-edit :defer t)

(use-package counsel
  :after ivy
  :defer t
  :config
  (counsel-mode)
  (setq ivy-initial-inputs-alist nil)) ; Disable the "^" in interactive counsel commands like M-x

(use-package ivy
  :defer 0.1
  :diminish
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :defer t
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-configure :display-transformer-fn 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :defer t)
(define-key evil-normal-state-map (kbd "/") 'swiper-isearch-backward)

(defalias 'yes-or-no-p 'y-or-n-p) ; Screw typing "yes", all my homies type 'y'
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)) ; Lets us use these buffers evilly
; Scrolling stuff
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq auto-window-vscroll nil)

(setq ring-bell-function 'ignore) ; Not a big fan of my IDE yelling at me
(setq visible-bell t) ; But I AM a big fan of my IDE flashing its lights at me
(setq-default evil-cross-lines nil) ; Vim style behavior when you reach the end of a line

(setq-default c-default-style "stroustrup"
	      c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode 1)
(defvaralias 'c-basic-offset 'tab-width)
(add-hook 'haskell-indentation-mode-hook (lambda () (interactive) (setq-default indent-tabs-mode 1)))
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <indent-for-tab-command>") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "<remap> <c-indent-line-or-region>") 'tab-to-tab-stop)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(use-package org-bullets :defer t)
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-hide-leading-stars t)

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
	org-src-window-setup 'current-window
	org-src-preserve-indentation t)

(use-package org-auto-tangle
  :ensure t
  :defer t)
(add-hook 'org-mode-hook (lambda () (interactive) (org-auto-tangle-mode 1)))

(use-package org-tempo
  :ensure nil)

(setq-default org-link-elisp-confirm-function nil)
(setq-default org-return-follows-link t)
(setq org-link-elisp-skip-confirm-regexp "\\`find-file*\\'")

(defun org/return ()
  "Call `org-return' then indent (if `electric-indent-mode' is on)."
  (interactive)
  (org-return electric-indent-mode))

(defun org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it"
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (ignore)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (ignore)
             (org-open-at-point arg))))

        (`paragraph
         (ignore))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (ignore)))))))

(defun org/shift-return (&optional arg)
  "Insert a literal newline, or dwim in tables.
Executes `org-table-copy-down' if in table."
  (interactive "p")
  (if (org-at-table-p)
      (org-table-copy-down arg)
    (org-return nil arg)))

(add-hook 'org-mode-hook (lambda ()
							 (evil-local-set-key 'insert (kbd "S-<return>") 'org/return)
							 (evil-local-set-key 'insert (kbd "<return>") 'org/shift-return)
							 (evil-local-set-key 'normal (kbd "<return>") 'org/dwim-at-point)))

; thanks dt for this one
(with-eval-after-load 'org-faces
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

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory "~/org/notes")
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-enable))

(setq org-agenda-files (list "~/org/agenda/todo.org"
							 "~/org/agenda/homework.org"
							 "~/org/agenda/emacs.org"
							 "~/org/agenda/schedule.org"))

(setq org-ellipsis " ▼ ")
(setq org-directory "~/org")

(use-package evil-collection
	:after evil
	:defer t
	:config
	(setq evil-collection-mode-list '(dashboard dired ibuffer search agenda))
	(evil-collection-init))

(use-package general
	:init
	(general-evil-setup t))

(defun bugger/kill-buffer ()
  (interactive)
  (when (buffer-modified-p)
	(when (y-or-n-p "Buffer modified. Save?")
	  (save-buffer)))
  (kill-buffer (buffer-name)))

(defun bugger/kill-buffer-and-window ()
  (interactive)
  (when (buffer-modified-p)
	(when (y-or-n-p "Buffer modified. Save?")
	  (save-buffer)))
  (kill-buffer-and-window))

(defun bugger/edit-src ()
  (interactive)
  (if (org-src-edit-buffer-p)
	  (org-edit-src-exit)
	(org-edit-special)))

(defun bugger/reload ()
  (interactive)
  (org-babel-tangle (concat user-emacs-directory "config.org"))
  (byte-compile-file (concat user-emacs-directory "init.el"))
  (load-file user-init-file)
  (load-file user-init-file))

(nvmap :prefix "SPC"
  "b" '(:ignore t :wk "buffer")
  "b i" '(ibuffer :which-key "Ibuffer")
  "b c" '(bugger/kill-buffer :which-key "Close the current buffer")
  "b k" '(bugger/kill-buffer-and-window :which-key "Close the current buffer and window")
  "b b" '(counsel-switch-buffer :which-key "Open a buffer in a new window")
  "b r" '(revert-buffer :which-key "Reload the buffer")
  "b s" '(switch-to-buffer "*scratch*" :which-key "Open the scratch buffer"))
(define-key evil-normal-state-map (kbd "q") 'bugger/kill-buffer)
(define-key evil-normal-state-map (kbd "Q") 'bugger/kill-buffer-and-window)

(nvmap :prefix "SPC"
  "w" '(:ignore t :which-key "window")
  "w v" '(evil-window-vsplit :which-key "Open a vertical split")
  "w w" '(evil-window-next :which-key "Switch to the next window")
  "w n" '(evil-window-new :which-key "Open a horizontal split")
  "w c" '(evil-window-delete :which-key "Close the current window")
  "w k" #'(lambda ()
			(interactive)
			(when (buffer-modified-p)
			  (when (y-or-n-p "Buffer modified. Save?")
				(save-buffer)))
			(kill-buffer-and-window) :which-key "Close the current buffer and window"))

(nvmap :prefix "SPC"
  "d" '(:ignore t :which-key "dired")
  "d" '(dired :which-key "Open dired")
  "j" '(dired-jump :which-key "Open dired in the current directory"))

(nvmap :prefix "SPC"
  "." '(find-file :which-key "Open a file")
  "f" '(:ignore t :which-key "file")
  "f s" '(save-buffer :which-key "Save file")
  "f r" '(counsel-recentf :which-key "List recent files to open")
  "f u" '(sudo-edit-find-file :which-key "Find file as root")
  "f U" '(sudo-edit :which-key "Edit as root"))

(nvmap :prefix "SPC"
  "o" '(:ignore t :which-key "org")
  "o b" '(:ignore t :which-key "babel")
  "o b e" '(bugger/edit-src :which-key "Edit a code block")
  "o b a" '(org-auto-tangle-mode :which-key "Toggle auto tangle mode")
  "o b t" '(org-babel-tangle :which-key "Tangle the current file")
  "o b k" '(org-edit-src-abort :which-key "Abort editing a code block"))

(nvmap :prefix "SPC"
  "o" '(:ignore t :which-key "org")
  "o t" '(:ignore t :which-key "toggle")
  "o t t" '(org-todo :which-key "Toggle todo"))

(nvmap :prefix "SPC o"
  "r" '(:ignore t :which-key "roam")
  "r f" '(org-roam-node-find :which-key "Open a note file")
  "r i" '(org-roam-node-insert :which-key "Insert a roam node")
  "r r" '(org-roam-buffer-toggle :which-key "Toggle org roam")
  "r v" '(org-roam-node-visit :which-key "Visit an org node")
  "r u" '(org-roam-db-sync :which-key "Update roam database")
  "r c" '(org-capture-finalize :which-key "Finish roam capture")
  "r a" '(org-capture-kill :which-key "Abort roam capture")
  "r n" '(org-capture-refile :which-key "Refile roam capture"))

(nvmap :prefix "SPC o"
  "a" '(:ignore t :which-key "agenda")
  "a t" '(org-todo-list :which-key "List TODO entries")
  "a a" '(org-agenda :which-key "Open org agenda")
  "a s" '(org-schedule :which-key "Schedule something"))

(nvmap :prefix "SPC"
  "h" '(:ignore t :which-key "help")
  "h f" '(describe-function :which-key "Describe a function")
  "h v" '(describe-variable :which-key "Describe a variable")
  "h k" '(describe-key :which-key "Describe what a key does")
  "h r" '(:ignore t :which-key "reload")
  "h r r" '(bugger/reload :which-key "init file"))

(nvmap :prefix "SPC"
  "e" '(:ignore t :which-key "evaluate")
  "e b" '(eval-buffer (current-buffer) :which-key "Evaluate current buffer")
  "e r" '(eval-region :which-key "Evaluate region"))

(nvmap :prefix "SPC"
  "b" '(:ignore t :which-key "bookmark")
  "b m" '(bookmark-set :which-key "Set a bookmark")
  "b o" '(bookmark-jump :which-key "Jump to a bookmark"))

(global-set-key (kbd "<escape>") 'abort-minibuffers)

(global-set-key (kbd "DEL") 'backward-delete-char)
(eval-after-load 'ivy #'(lambda ()
						  (define-key ivy-mode-map (kbd "DEL") 'ivy-backward-delete-char)))
(setq c-backspace-function 'backward-delete-char)

(global-set-key (kbd "C-j") #'(lambda ()
								(interactive)
								(evil-scroll-down 1)))
(define-key evil-normal-state-map (kbd "<remap> <org-return-and-maybe-indent>") #'(lambda ()
								(interactive)
								(evil-scroll-down 1)))
(global-set-key (kbd "C-k") #'(lambda ()
								(interactive)
								(evil-scroll-up 1)))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)) ; use dired-find-file instead if not using dired-open package

(with-eval-after-load 'ibuffer
  (evil-define-key 'normal ibuffer-mode-map (kbd "l") 'ibuffer-visit-buffer))

(define-key evil-normal-state-map (kbd "TAB") 'evil-toggle-fold)

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(setq gc-cons-threshold (* 2 1000 1000))
