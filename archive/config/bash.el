(dolist (item '(flycheck))
  (add-to-list 'slb/packages item))

(defun slb/setup-bash ()
  (add-hook 'sh-mode-hook 'flycheck-mode))

(if (package-installed-p 'flycheck)
    (slb/setup-bash)
  (add-hook 'slb/packages-installed 'slb/setup-bash))

;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
