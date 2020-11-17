;;; local-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "lisp/pianobar" "lisp/pianobar.el" (22751 59485
;;;;;;  0 0))
;;; Generated autoloads from lisp/pianobar.el

(autoload 'pianobar-key-setup "lisp/pianobar" "\
Setup the KEY map for telling pianobar what to do.

\(fn &optional KEY)" nil nil)

(autoload 'pianobar "lisp/pianobar" "\
Run an inferior pianobar process, input and output via buffer `*pianobar*'.
If there is a process already running in `*pianobar*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `pianobar-program-command').
Runs the hook `pianobar-mode-hook' (after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn)" t nil)

;;;***

(provide 'local-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; local-autoloads.el ends here
