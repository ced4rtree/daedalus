;;; flymake-elisp-config-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-elisp-config" "flymake-elisp-config.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flymake-elisp-config.el

(autoload 'flymake-elisp-config-mode "flymake-elisp-config" "\
Provide configurable `load-path' with flymake in Emacs Lisp mode.
Set getter function of `load-path' to `flymake-elisp-config-load-path-getter'.

This is a minor mode.  If called interactively, toggle the
`Flymake-Elisp-Config mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `flymake-elisp-config-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'flymake-elisp-config-global-mode 'globalized-minor-mode t)

(defvar flymake-elisp-config-global-mode nil "\
Non-nil if Flymake-Elisp-Config-Global mode is enabled.
See the `flymake-elisp-config-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flymake-elisp-config-global-mode'.")

(custom-autoload 'flymake-elisp-config-global-mode "flymake-elisp-config" nil)

(autoload 'flymake-elisp-config-global-mode "flymake-elisp-config" "\
Toggle Flymake-Elisp-Config mode in all buffers.
With prefix ARG, enable Flymake-Elisp-Config-Global mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Flymake-Elisp-Config mode is enabled in all buffers where
`flymake-elisp-config-mode' would do it.

See `flymake-elisp-config-mode' for more information on
Flymake-Elisp-Config mode.

\(fn &optional ARG)" t nil)

(defvar flymake-elisp-config-auto-mode nil "\
Non-nil if Flymake-Elisp-Config-Auto mode is enabled.
See the `flymake-elisp-config-auto-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flymake-elisp-config-auto-mode'.")

(custom-autoload 'flymake-elisp-config-auto-mode "flymake-elisp-config" nil)

(autoload 'flymake-elisp-config-auto-mode "flymake-elisp-config" "\
Configure flymake appropriately in Emacs Lisp file.
`flymake-elisp-config-global-mode' should be turned on to use this minor mode.

This is a minor mode.  If called interactively, toggle the
`Flymake-Elisp-Config-Auto mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='flymake-elisp-config-auto-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'flymake-elisp-config-as-default "flymake-elisp-config" "\
Current buffer file is regarded as usual Emacs Lisp file.
`load-path' used by flymake is provided by
`elisp-flymake-byte-compile-load-path'." t nil)

(autoload 'flymake-elisp-config-as-config "flymake-elisp-config" "\
Current buffer file is regarded as Emacs configuration file by flymake.
`load-path' used by flymake is provided by
`flymake-elisp-config-get-load-path-config'." t nil)

(autoload 'flymake-elisp-config-as-cask "flymake-elisp-config" "\
Current buffer file is regarded as a `cask'-managed project by flymake.
`load-path' used by flymake is provided by
`flymake-elisp-config-get-load-path-cask'." t nil)

(autoload 'flymake-elisp-config-as-keg "flymake-elisp-config" "\
Current buffer file is regarded as a `keg'-managed project by flymake.
`load-path' used by flymake is provided by
`flymake-elisp-config-get-load-path-keg'." t nil)

(register-definition-prefixes "flymake-elisp-config" '("flymake-elisp-config-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-elisp-config-autoloads.el ends here
