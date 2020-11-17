;; -*- lexical-binding: t  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'use-package))

(defun slb/xterm-title-update ()
  (unless (active-minibuffer-window)
    (send-string-to-terminal (concat "\033]1;" (buffer-name) "\007"))
    (if buffer-file-name
        (send-string-to-terminal (concat "\033]2;" (abbreviate-file-name (buffer-file-name)) "\007"))
      (send-string-to-terminal (concat "\033]2;" (buffer-name) "\007")))))

(defun slb/xterm-title-update--register ()
  (add-hook 'after-init-hook 'slb/xterm-title-update)
  (add-hook 'post-command-hook 'slb/xterm-title-update))

(add-hook 'terminal-init-xterm-hook 'slb/xterm-title-update--register)

(use-package winner
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; comment-column: 64
;; fill-column: 75
;; End:
