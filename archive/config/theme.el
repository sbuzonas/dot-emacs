(dolist (item '(mustard-theme color-theme-sanityinc-tomorrow))
  (add-to-list 'slb/packages item))

(defun slb/initialize-theme ()
  (interactive)
  (color-theme-sanityinc-tomorrow-eighties))
;;  (color-theme-sanityinc-tomorrow-night))
;;  (load-theme 'mustard))

(if (package-installed-p 'color-theme-sanityinc-tomorrow)
    (slb/initialize-theme)
  (add-hook 'slb/packages-installed 'slb/initialize-theme))

;; Local variables:
;; mode: emacs-lisp
;; comment-column: 64
;; end:
