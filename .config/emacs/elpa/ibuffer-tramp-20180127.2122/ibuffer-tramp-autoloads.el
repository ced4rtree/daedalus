;;; ibuffer-tramp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ibuffer-tramp" "ibuffer-tramp.el" (0 0 0 0))
;;; Generated autoloads from ibuffer-tramp.el

(autoload 'ibuffer-tramp-generate-filter-groups-by-tramp-connection "ibuffer-tramp" "\
Create a set of ibuffer filter groups based on the TRAMP connection of buffers" nil nil)

(autoload 'ibuffer-tramp-set-filter-groups-by-tramp-connection "ibuffer-tramp" "\
Set the current filter groups to filter by TRAMP connection." t nil)

(register-definition-prefixes "ibuffer-tramp" '("ibuffer-tramp-connection" "tramp-connection"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ibuffer-tramp-autoloads.el ends here
