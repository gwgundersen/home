;;; speedbar-git-respect-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "speedbar-git-respect" "speedbar-git-respect.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from speedbar-git-respect.el

(defvar speedbar-git-respect-mode nil "\
Non-nil if Speedbar-Git-Respect mode is enabled.
See the `speedbar-git-respect-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `speedbar-git-respect-mode'.")

(custom-autoload 'speedbar-git-respect-mode "speedbar-git-respect" nil)

(autoload 'speedbar-git-respect-mode "speedbar-git-respect" "\
Toggle speedbar-git-respect mode

This is a minor mode.  If called interactively, toggle the
`Speedbar-Git-Respect mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='speedbar-git-respect-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "speedbar-git-respect" '("speedbar-git-respect--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; speedbar-git-respect-autoloads.el ends here
