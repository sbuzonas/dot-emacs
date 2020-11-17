(defun xterm-title-update ()
  (interactive)
  (unless (active-minibuffer-window)
    (send-string-to-terminal (concat "\033]1;" (buffer-name) "\007"))
    (if buffer-file-name
        (send-string-to-terminal (concat "\033]2;" (abbreviate-file-name (buffer-file-name)) "\007"))
      (send-string-to-terminal (concat "\033]2;" (buffer-name) "\007")))))
(add-hook 'after-init-hook 'xterm-title-update)
(add-hook 'post-command-hook 'xterm-title-update)

;; Local variables:
;; mode: emacs-lisp
;; comment-folumn: 64
;; end:
