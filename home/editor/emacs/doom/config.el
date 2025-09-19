;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; font
(setq doom-font (font-spec :family "Iosevka Nerd Font Propo" :size 18))

;; theme
(setq doom-theme 'doom-one)

;; line numbers
(setq display-line-numbers-type t)

;; scroll margin
(setq scroll-margin 8)

;; org stuff
(setq org-directory "~/org/")

;; tab-bar workflow
(tab-bar-mode t)
(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))

(defun cedar/tab-name (tab)
  "Returns the name of TAB as a string."
  (cdr (assoc-string 'name tab)))

(defun cedar/open-name-in-tab (name always-perform-callback callback &rest callback-args)
  "Open/create a tab called NAME, and call CALLBACK upon opening.

If NAME is already a tab that exists, switch to it.  If there's not a
tab with the name NAME, then create a new tab with the name NAME and
call CALLBACK with the optionally supplied CALLBACK-ARGS.

If ALWAYS-PERFORM-CALLBACK is t, CALLBACK will always be performed with
its arguments, even if NAME is already an existing tab."

  (if (and (eq (length (tab-bar-tabs)) 1)
           (string-equal (cedar/tab-name (car (tab-bar-tabs))) "*doom*"))
      (progn
        (tab-rename name)
        (apply callback callback-args))
    (let* ((tab-names (mapcar #'cedar/tab-name (tab-bar-tabs))))
      (if (and (member name tab-names) (not always-perform-callback))
          (tab-bar-switch-to-tab name)
        (progn
          (tab-bar-switch-to-tab name)
          (apply callback callback-args))))))

;; project.el configuration
(use-package! project
  :ensure nil
  :commands (project-prompt-project-dir)
  :config
  (defun cedar/project-switch-project-tab ()
    "Switch to a project tab, or create one if the prompted project doesn't exist."
    (interactive)
    (let* ((project-dir (project-prompt-project-dir))
           (project-dir-split (split-string project-dir "/"))
           ;; project-dir-split formatted as ("dir1" "dir2" "..." "project" "")
           ;; find just the string for the project name by getting the second to last element
           (project-name-raw (nth (- (length project-dir-split) 2) project-dir-split))
           (project-name (if (string= (substring project-name-raw 0 1) ".")
                             project-name-raw
                           (capitalize project-name-raw))))
      (cedar/open-name-in-tab project-name nil 'project-switch-project project-dir)))

  (defun cedar/project-kill-buffers-and-tab ()
    "Kill all buffers in the current project and close the current tab."
    (interactive)
    (project-kill-buffers)
    ;; when the only tab open is a project, blindly closing it leaves
    ;; you on *scratch* but doesn't rename the buffer, which messes
    ;; with some tab opening settings
    (if (length> (tab-bar-tabs) 1)
        (tab-bar-close-tab)
      (when (string-equal (buffer-name) "*doom*")
        (tab-bar-rename-tab "*doom*"))))
  :bind (("C-x p p" . cedar/project-switch-project-tab)
         ("C-x p k" . cedar/project-kill-buffers-and-tab)))

;; emms
(use-package! emms
  :defer t
  :commands (emms-all emms-smart-browse)
  :defines emms-playlist-mode-map
  :custom
  (emms-directory (concat doom-data-dir "emms"))
  (emms-cache-file (concat doom-cache-dir "emms"))
  (emms-seek-seconds 5)
  (emms-player-mpd-music-directory (concat (getenv "HOME") "/Music"))
  (emms-player-mpd-server-name "localhost")
  (emms-player-mpd-server-port "6600")
  (mpc-host "localhost:6600")
  (emms-player-list '(emms-player-mpd))
  (emms-info-functions '(emms-info-mpd))

  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)

  (emms-all)
  (emms-player-mpd-connect)
  (emms-mode-line-disable)
  ;; (emms-mpris-enable)

  (defun cedar/emms-smart-browse-in-tab ()
    (interactive)
    (cedar/open-name-in-tab "EMMS (Music)" nil #'emms-smart-browse))

  :bind (("C-c m t" . emms-pause) ;; t for toggle
         ("C-c m n" . emms-next)
         ("C-c m p" . emms-previous)
         ("C-c m m" . cedar/emms-smart-browse-in-tab)
         :map emms-playlist-mode-map
         ("Z" . emms-shuffle)))

;; indent guides
(after! indent-bars
  (setq indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
        indent-bars-color '(highlight :face-bg t :blend 0.7)
        indent-bars-highlight-current-depth '(:pattern "."))
  (add-hook! 'emacs-lisp-mode (indent-bars-mode 0)))

(after! dirvish
  (map! :map dired-mode-map
        (:when (not (modulep! :editor evil)))
        "C-b" 'backward-char
        "C-f" 'forward-char
        "b" 'dired-up-directory)
  (map! :map dirvish-mode-map
        (:when (not (modulep! :editor evil)))
        "f" 'dired-find-file))

;; pretty rainbows
(use-package! colorful-mode
  :custom
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package! eglot-java
  :defer t
  :hook (eglot-managed . (lambda () (when (or (string= major-mode "java-mode")
                                              (string= major-mode "java-ts-mode"))
                                      (eglot-java-mode t)))))

;; email
(after! auth-source
  (defun efs/lookup-password (&rest keys)
    "Lookup a password from ~/.authinfo.gpg using KEYS to index the desired password.

  e.g. (efs/lookup-password :host \"example.com\" :user \"user\"), which
  will find the password for user@example.com"

    (let ((result (apply #'auth-source-search keys)))
      (when result
        (funcall (plist-get (car result) :secret))))))

(load-file (concat doom-local-dir "emails.el"))
(defun cedar/open-mu4e-in-tab ()
  "Open mu4e in a new tab. See cedar/open-name-in-tab."
  (interactive)
  (cedar/open-name-in-tab "MU4E (Mail)" nil #'=mu4e))
(map! :leader
      "M M" #'cedar/open-mu4e-in-tab
      "o m" #'cedar/open-mu4e-in-tab)
(after! mu4e
  (setq mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask))

;; org agenda/calfw stuff
(setq org-agenda-files `(,(concat org-directory "/agenda/")))
(defun cedar/open-agenda-in-tab (view)
  "Go to an org agenda tab, creating one if it doesn't exist.

The parameter VIEW is used to determine whether the tab should open to
org-agenda or calfw, and can be set to the symbol of either either
agenda or calendar.

Example: (cedar/open-agenda-in-tab \='calendar)"
  (interactive)
  (let* ((agenda-view (if (equal view 'agenda)
                          #'org-agenda
                        #'=calendar)))
    (cedar/open-name-in-tab "Agenda" t agenda-view nil "n")))
(map! :leader
      "o a" #'cedar/open-agenda-in-tab
      "o c" #')
